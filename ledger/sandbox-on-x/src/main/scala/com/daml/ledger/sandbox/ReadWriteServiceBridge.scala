// Copyright (c) 2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.ledger.sandbox

import akka.NotUsed
import akka.stream.scaladsl.Source
import akka.stream.{BoundedSourceQueue, Materializer, QueueOfferResult}
import com.daml.daml_lf_dev.DamlLf.Archive
import com.daml.error.definitions.LedgerApiErrors
import com.daml.error.{ContextualizedErrorLogger, NoLogging}
import com.daml.ledger.api.health.{HealthStatus, Healthy}
import com.daml.ledger.configuration.{
  Configuration,
  LedgerId,
  LedgerInitialConditions,
  LedgerTimeModel,
}
import com.daml.ledger.offset.Offset
import com.daml.ledger.participant.state.index.v2.ContractStore
import com.daml.ledger.participant.state.v2.Update.CommandRejected.FinalReason
import com.daml.ledger.participant.state.v2._
import com.daml.ledger.sandbox.ReadWriteServiceBridge.Submission._
import com.daml.lf.data.Time.Timestamp
import com.daml.lf.data.{Ref, Time}
import com.daml.lf.engine.Blinding
import com.daml.lf.transaction.Transaction.{KeyActive, KeyCreate, NegativeKeyLookup}
import com.daml.lf.transaction.{BlindingInfo, CommittedTransaction, GlobalKey, SubmittedTransaction}
import com.daml.logging.{ContextualizedLogger, LoggingContext}
import com.daml.metrics.{InstrumentedSource, Metrics, Timed}
import com.daml.platform.apiserver.execution.MissingContracts
import com.daml.platform.store.appendonlydao.events._
import com.daml.telemetry.TelemetryContext
import com.google.common.primitives.Longs
import com.google.rpc.code.Code
import com.google.rpc.status.Status

import java.util.UUID
import java.util.concurrent.atomic.{AtomicLong, AtomicReference}
import java.util.concurrent.{CompletableFuture, CompletionStage}
import scala.collection.Searching
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import scala.util.chaining._

case class ReadWriteServiceBridge(
    participantId: Ref.ParticipantId,
    ledgerId: LedgerId,
    maxDedupSeconds: Int,
    submissionBufferSize: Int,
    contractStore: AtomicReference[Option[ContractStore]],
    metrics: Metrics,
)(implicit
    mat: Materializer,
    loggingContext: LoggingContext,
    servicesExecutionContext: ExecutionContext,
) extends ReadService
    with WriteService
    with AutoCloseable {
  private val offsetIdx = new AtomicLong(0L)

  import ReadWriteServiceBridge._

  private[this] val logger = ContextualizedLogger.get(getClass)

  private type KeyInputs = Either[
    com.daml.lf.transaction.Transaction.KeyInputError,
    Map[Key, com.daml.lf.transaction.Transaction.KeyInput],
  ]

  private type SequencerQueue =
    Vector[(Offset, (Submission.Transaction, Map[GlobalKey, Option[ContractId]], Set[ContractId]))]

  private val (conflictCheckingQueue, source) = {
    val parallelCheck = InstrumentedSource
      .queue[Transaction](
        bufferSize = 1024,
        capacityCounter = metrics.daml.SoX.conflictQueueCapacity,
        lengthCounter = metrics.daml.SoX.conflictQueueLength,
        delayTimer = metrics.daml.SoX.conflictQueueDelay,
      )
      .map { tx =>
        val enqueuedAt = contractStore
          .get()
          .getOrElse(throw new RuntimeException("ContractStore not there yet"))
          .cacheOffset()
        enqueuedAt -> tx
      }
      .mapAsync(64) { case (enqueuedAt, tx) =>
        Timed.future(
          metrics.daml.SoX.parallelConflictCheckingDuration,
          Future {
            (
              tx.transaction.contractKeyInputs,
              tx.transaction.inputContracts,
              tx.transaction.updatedContractKeys,
              tx.transaction.consumedContracts,
            )
          }.flatMap { case (keyInputs, inputContracts, updatedKeys, consumedContracts) =>
            parallelConflictCheck(tx, inputContracts, keyInputs).map(tx =>
              (enqueuedAt, keyInputs, inputContracts, updatedKeys, consumedContracts, tx)
            )
          },
        )
      }

    InstrumentedSource
      .bufferedSource(
        parallelCheck,
        metrics.daml.SoX.queueBeforeSequencer,
        128,
      )
      .statefulMapConcat(sequence)
      .preMaterialize()
  }

  case class SequencerQueueState(
      sequencerQueue: SequencerQueue = Vector.empty,
      keyState: Map[Key, (Option[ContractId], Offset)] = Map.empty,
      consumedContractsState: Map[ContractId, Offset] = Map.empty,
  )(metrics: Metrics) {

    def enqueue(
        offset: Offset,
        transaction: Transaction,
        updatedKeys: Map[Key, Option[ContractId]],
        consumedContracts: Set[ContractId],
    ): SequencerQueueState =
      Timed.value(
        metrics.daml.SoX.stateEnqueue,
        SequencerQueueState(
          sequencerQueue =
            (sequencerQueue :+ (offset -> (transaction, updatedKeys, consumedContracts)))
              .tap(q => metrics.daml.SoX.sequencerQueueLengthCounter.update(q.length)),
          keyState = (keyState ++ updatedKeys.view.mapValues(_ -> offset)).tap(s =>
            metrics.daml.SoX.keyStateSize.update(s.size)
          ),
          consumedContractsState =
            (consumedContractsState ++ consumedContracts.view.map(_ -> offset)).tap(s =>
              metrics.daml.SoX.consumedContractsStateSize.update(s.size)
            ),
        )(metrics),
      )

    def dequeue(upToOffset: Offset): SequencerQueueState =
      Timed.value(
        metrics.daml.SoX.stateDequeue, {
          val pruneAfter = Timed.value(
            metrics.daml.SoX.queueSearch,
            sequencerQueue.view.map(_._1).search(upToOffset) match {
              case Searching.Found(foundIndex) => foundIndex + 1
              case Searching.InsertionPoint(insertionPoint) => insertionPoint
            },
          )

          SequencerQueueState(
            sequencerQueue = Timed.value(
              metrics.daml.SoX.slice,
              sequencerQueue.drop(pruneAfter),
            ),
            keyState = sequencerQueue.take(pruneAfter).foldLeft(keyState) {
              case (ks, (enqueuedAt, (_, updatedKeys, _))) =>
                updatedKeys.foldLeft(ks) { case (kse, (key, _)) =>
                  kse.get(key).fold(throw new RuntimeException("Should not be missing")) {
                    case (_, offset) if enqueuedAt == offset => kse - key
                    case _ => kse
                  }
                }
            },
            consumedContractsState =
              sequencerQueue.take(pruneAfter).foldLeft(consumedContractsState) {
                case (css, (enqueuedAt, (_, _, cc))) =>
                  cc.foldLeft(css) { case (csss, cId) =>
                    csss.get(cId).fold(throw new RuntimeException("Should not be missing")) {
                      case offset if offset == enqueuedAt => csss - cId
                      case _ => csss
                    }
                  }
              },
          )(metrics)
        },
      )
  }

  private def sequence: () => (
      (
          Offset,
          KeyInputs,
          Set[ContractId],
          Map[GlobalKey, Option[ContractId]],
          Set[ContractId],
          Either[SoxRejection, Transaction],
      )
  ) => Iterable[
    (Offset, Update)
  ] = () => {
    @volatile var sequencerQueueState: SequencerQueueState = SequencerQueueState()(metrics)

    {
      case (_, _, _, _, _, Left(rejection)) =>
        Timed.value(
          metrics.daml.SoX.sequenceDuration, {
            val newIndex = offsetIdx.getAndIncrement()
            val newOffset = toOffset(newIndex)
            Iterable(newOffset -> toRejection(rejection))
          },
        )
      case (
            enqueuedAt,
            keyInputs,
            inputContracts,
            updatedKeys,
            consumedContracts,
            Right(transaction),
          ) =>
        Timed.value(
          metrics.daml.SoX.sequenceDuration, {
            val newIndex = offsetIdx.getAndIncrement()
            val newOffset = toOffset(newIndex)
            conflictCheckSlice(
              sequencerQueueState,
              keyInputs,
              inputContracts,
              transaction,
            ) match {
              case Left(rejection) => Iterable(newOffset -> toRejection(rejection))
              case Right(acceptedTx) =>
                sequencerQueueState = sequencerQueueState
                  .dequeue(enqueuedAt)
                  .enqueue(newOffset, transaction, updatedKeys, consumedContracts)

                Iterable(newOffset -> successMapper(acceptedTx, newIndex, participantId))
            }
          },
        )
    }
  }

  def invalidInputFromParticipant(
      originalTx: Submission.Transaction
  ): com.daml.lf.transaction.Transaction.KeyInputError => SoxRejection = {
    case com.daml.lf.transaction.Transaction.InconsistentKeys(key) =>
      GenericRejectionFailure(s"Inconsistent keys for $key")(originalTx)
    case com.daml.lf.transaction.Transaction.DuplicateKeys(key) =>
      GenericRejectionFailure(s"Duplicate key for $key")(originalTx)
  }

  private def conflictCheckSlice(
      sequencerQueueState: SequencerQueueState,
      keyInputs: KeyInputs,
      inputContracts: Set[ContractId],
      transaction: Submission.Transaction,
  ): Either[SoxRejection, Submission.Transaction] = {
    val updatedKeys = sequencerQueueState.keyState
    val archives = sequencerQueueState.consumedContractsState

    keyInputs.left
      .map(invalidInputFromParticipant(transaction))
      .flatMap(_.foldLeft[Either[SoxRejection, Unit]](Right(())) {
        case (Right(_), (key, KeyCreate)) =>
          updatedKeys.get(key) match {
            case Some((None, _)) | None => Right(())
            case Some((Some(_), _)) => Left(DuplicateKey(key)(transaction))
          }
        case (Right(_), (key, NegativeKeyLookup)) =>
          updatedKeys.get(key) match {
            case Some((None, _)) | None => Right(())
            case Some((Some(actual), _)) =>
              Left(InconsistentContractKey(None, Some(actual))(transaction))
          }
        case (Right(_), (key, KeyActive(cid))) =>
          updatedKeys.get(key) match {
            case Some((Some(`cid`), _)) | None => Right(())
            case Some((other, _)) => Left(InconsistentContractKey(other, Some(cid))(transaction))
          }
        case (left, _) => left
      })
      .flatMap { _ =>
        val alreadyArchived = inputContracts.intersect(archives.keySet)
        if (alreadyArchived.nonEmpty) Left(UnknownContracts(alreadyArchived)(transaction))
        else Right(())
      }
      .map(_ => transaction)
  }

  private def toRejection(rejection: SoxRejection) =
    Update.CommandRejected(
      recordTime = Timestamp.now(),
      completionInfo = rejection.originalTx.submitterInfo.toCompletionInfo,
      reasonTemplate = FinalReason(rejection.toStatus),
    )

  private def parallelConflictCheck(
      transaction: Submission.Transaction,
      inputContracts: Set[ContractId],
      keyInputs: KeyInputs,
  ): Future[Either[SoxRejection, Submission.Transaction]] =
    validateCausalMonotonicity(
      transaction,
      inputContracts,
      transaction.transactionMeta.ledgerEffectiveTime,
      transaction.blindingInfo.divulgence.keySet,
    ).flatMap {
      case Right(_) => validateKeyUsages(transaction, keyInputs)
      case rejection => Future.successful(rejection)
    }.flatMap {
      case Right(_) => validatePartyAllocation(transaction.transaction)
      case rejection => Future.successful(rejection)
    }.map(_.map(_ => transaction))

  private def validatePartyAllocation(
      transaction: SubmittedTransaction
  ): Future[Either[SoxRejection, Unit]] = {
    val _ = transaction
    // TODO implement
    Future.successful(Right(()))
  }

  private def validateCausalMonotonicity(
      transaction: Transaction,
      inputContracts: Set[ContractId],
      transactionLedgerEffectiveTime: Timestamp,
      divulged: Set[ContractId],
  ): Future[Either[SoxRejection, Unit]] = {
    val referredContracts = inputContracts.diff(divulged)
    if (referredContracts.isEmpty) {
      Future.successful(Right(()))
    } else
      contractStore
        .get()
        .getOrElse(throw new RuntimeException("ContractStore not there yet"))
        .lookupMaximumLedgerTime(referredContracts)
        .transform {
          case Failure(MissingContracts(missingContractIds)) =>
            Success(Left(UnknownContracts(missingContractIds)(transaction)))
          case Failure(err) => Success(Left(GenericRejectionFailure(err.getMessage)(transaction)))
          case Success(maximumLedgerEffectiveTime) =>
            maximumLedgerEffectiveTime
              .filter(_ > transactionLedgerEffectiveTime)
              .fold[Try[Either[SoxRejection, Unit]]](Success(Right(())))(
                contractLedgerEffectiveTime =>
                  Success(
                    Left(
                      CausalMonotonicityViolation(
                        contractLedgerEffectiveTime = contractLedgerEffectiveTime,
                        transactionLedgerEffectiveTime = transactionLedgerEffectiveTime,
                      )(transaction)
                    )
                  )
              )
        }
  }

  override def isApiDeduplicationEnabled: Boolean = true

  override def submitTransaction(
      submitterInfo: SubmitterInfo,
      transactionMeta: TransactionMeta,
      transaction: SubmittedTransaction,
      estimatedInterpretationCost: Long,
  )(implicit
      loggingContext: LoggingContext,
      telemetryContext: TelemetryContext,
  ): CompletionStage[SubmissionResult] =
    toSubmissionResult(
      conflictCheckingQueue.offer(
        Submission.Transaction(
          submitterInfo = submitterInfo,
          transactionMeta = transactionMeta,
          transaction = transaction,
          estimatedInterpretationCost = estimatedInterpretationCost,
          blindingInfo = Blinding.blind(transaction),
        )
      )
    )

  override def submitConfiguration(
      maxRecordTime: Time.Timestamp,
      submissionId: Ref.SubmissionId,
      config: Configuration,
  )(implicit
      loggingContext: LoggingContext,
      telemetryContext: TelemetryContext,
  ): CompletionStage[SubmissionResult] =
    submit(
      Submission.Config(
        maxRecordTime = maxRecordTime,
        submissionId = submissionId,
        config = config,
      )
    )

  override def currentHealth(): HealthStatus = Healthy

  override def allocateParty(
      hint: Option[Ref.Party],
      displayName: Option[String],
      submissionId: Ref.SubmissionId,
  )(implicit
      loggingContext: LoggingContext,
      telemetryContext: TelemetryContext,
  ): CompletionStage[SubmissionResult] =
    submit(
      Submission.AllocateParty(
        hint = hint,
        displayName = displayName,
        submissionId = submissionId,
      )
    )

  override def uploadPackages(
      submissionId: Ref.SubmissionId,
      archives: List[Archive],
      sourceDescription: Option[String],
  )(implicit
      loggingContext: LoggingContext,
      telemetryContext: TelemetryContext,
  ): CompletionStage[SubmissionResult] =
    submit(
      Submission.UploadPackages(
        submissionId = submissionId,
        archives = archives,
        sourceDescription = sourceDescription,
      )
    )

  override def prune(
      pruneUpToInclusive: Offset,
      submissionId: Ref.SubmissionId,
      pruneAllDivulgedContracts: Boolean,
  ): CompletionStage[PruningResult] =
    CompletableFuture.completedFuture(
      PruningResult.ParticipantPruned
    )

  override def ledgerInitialConditions(): Source[LedgerInitialConditions, NotUsed] =
    Source.single(
      LedgerInitialConditions(
        ledgerId = ledgerId,
        config = Configuration(
          generation = 1L,
          timeModel = LedgerTimeModel.reasonableDefault,
          maxDeduplicationTime = java.time.Duration.ofSeconds(maxDedupSeconds.toLong),
        ),
        initialRecordTime = Timestamp.now(),
      )
    )

  var stateUpdatesWasCalledAlready = false
  override def stateUpdates(
      beginAfter: Option[Offset]
  )(implicit loggingContext: LoggingContext): Source[(Offset, Update), NotUsed] = {
    // TODO for PoC purposes:
    //   This method may only be called once, either with `beginAfter` set or unset.
    //   A second call will result in an error unless the server is restarted.
    //   Bootstrapping the bridge from indexer persistence is supported.
    synchronized {
      if (stateUpdatesWasCalledAlready)
        throw new IllegalStateException("not allowed to call this twice")
      else stateUpdatesWasCalledAlready = true
    }
    logger.info("Indexer subscribed to state updates.")
    beginAfter.foreach(offset =>
      logger.warn(
        s"Indexer subscribed from a specific offset $offset. This offset is not taking into consideration, and does not change the behavior of the ReadWriteServiceBridge. Only valid use case supported: service starting from an already ingested database, and indexer subscribes from exactly the ledger-end."
      )
    )

    queueSource.merge(source)
  }

  val (queue: BoundedSourceQueue[Submission], queueSource: Source[(Offset, Update), NotUsed]) =
    Source
      .queue[Submission](submissionBufferSize)
      .map { submission =>
        val newOffsetIdx = offsetIdx.getAndIncrement()
        val newOffset = toOffset(newOffsetIdx)
        (newOffset, successMapper(submission, newOffsetIdx, participantId))
      }
      .preMaterialize()

  logger.info(
    s"BridgeLedgerFactory initialized. Configuration: [maxDedupSeconds: $maxDedupSeconds, submissionBufferSize: $submissionBufferSize]"
  )

  private def submit(submission: Submission): CompletionStage[SubmissionResult] =
    toSubmissionResult(queue.offer(submission))

  private def validateKeyUsages(
      transaction: Transaction,
      keyInputs: KeyInputs,
  ): Future[Either[SoxRejection, Unit]] = {
    val readers = transaction.transaction.informees // Is it informees??
    keyInputs.fold(
      err => Future.successful(Left(GenericRejectionFailure(err.toString)(transaction))),
      _.foldLeft(Future.successful[Either[SoxRejection, Unit]](Right(()))) {
        case (f, (key, inputState)) =>
          f.flatMap {
            case Right(_) =>
              contractStore
                .get()
                .getOrElse(throw new RuntimeException("ContractStore not there yet"))
                .lookupContractKey(readers, key)
                .map {
                  lookupResult =>
                    (inputState, lookupResult) match {
                      case (NegativeKeyLookup, Some(actual)) =>
                        Left(InconsistentContractKey(None, Some(actual))(transaction))
                      case (KeyCreate, Some(_)) =>
                        Left(DuplicateKey(key)(transaction))
                      case (KeyActive(expected), actual) if !actual.contains(expected) =>
                        Left(InconsistentContractKey(Some(expected), actual)(transaction))
                      case _ => Right(())
                    }
                }
            case left => Future.successful(left)
          }
      },
    )
  }

  override def close(): Unit = {
    logger.info("Shutting down BridgeLedgerFactory.")
    queue.complete()
  }
}

object ReadWriteServiceBridge {
  private implicit val errorLoggingContext: ContextualizedErrorLogger = NoLogging
  private val useSelfServiceErrorCodes = false

  trait Submission
  object Submission {
    sealed trait SoxRejection extends Submission {
      def toStatus: Status

      val originalTx: Submission.Transaction
    }
    final case class DuplicateKey(key: GlobalKey)(override val originalTx: Transaction)
        extends SoxRejection {
      override def toStatus: Status = if (!useSelfServiceErrorCodes)
        Status.of(Code.ABORTED.value, "Invalid contract key", Seq.empty)
      else
        LedgerApiErrors.ConsistencyErrors.DuplicateContractKey
          .RejectWithContractKeyArg("DuplicateKey: contract key is not unique", key)
          .rpcStatus(None)
    }

    final case class InconsistentContractKey(
        expectation: Option[ContractId],
        result: Option[ContractId],
    )(override val originalTx: Transaction)
        extends SoxRejection {
      override def toStatus: Status = if (!useSelfServiceErrorCodes)
        Status.of(Code.ABORTED.value, "Invalid contract key", Seq.empty)
      else
        LedgerApiErrors.ConsistencyErrors.InconsistentContractKey
          .Reject(
            s"Contract key lookup with different results. Expected: $expectation, result: $result"
          )
          .rpcStatus(None)
    }

    final case class GenericRejectionFailure(details: String)(override val originalTx: Transaction)
        extends SoxRejection {
      override def toStatus: Status = if (!useSelfServiceErrorCodes)
        Status.of(Code.ABORTED.value, "Invalid contract key", Seq.empty)
      else
        // TODO wrong error
        LedgerApiErrors.InternalError.VersionService(details).rpcStatus(None)
    }

    final case class CausalMonotonicityViolation(
        contractLedgerEffectiveTime: Timestamp,
        transactionLedgerEffectiveTime: Timestamp,
    )(override val originalTx: Transaction)
        extends SoxRejection {
      override def toStatus: Status = if (!useSelfServiceErrorCodes)
        Status.of(Code.ABORTED.value, "ADD DETAILS FOR LET failure", Seq.empty)
      else
        LedgerApiErrors.ConsistencyErrors.InvalidLedgerTime
          .RejectSimple("ADD DETAILS FOR LET failure")
          .rpcStatus(None)
    }

    final case class UnknownContracts(ids: Set[ContractId])(override val originalTx: Transaction)
        extends SoxRejection {
      override def toStatus: Status = if (!useSelfServiceErrorCodes)
        Status.of(Code.ABORTED.value, "Invalid contract key", Seq.empty)
      else
        LedgerApiErrors.ConsistencyErrors.ContractNotFound
          .MultipleContractsNotFound(ids.map(_.coid))
          .rpcStatus(None)
    }

    case class Transaction(
        submitterInfo: SubmitterInfo,
        transactionMeta: TransactionMeta,
        transaction: SubmittedTransaction,
        estimatedInterpretationCost: Long,
        blindingInfo: BlindingInfo,
    ) extends Submission
    case class Config(
        maxRecordTime: Time.Timestamp,
        submissionId: Ref.SubmissionId,
        config: Configuration,
    ) extends Submission
    case class AllocateParty(
        hint: Option[Ref.Party],
        displayName: Option[String],
        submissionId: Ref.SubmissionId,
    ) extends Submission

    case class UploadPackages(
        submissionId: Ref.SubmissionId,
        archives: List[Archive],
        sourceDescription: Option[String],
    ) extends Submission

    case class Rejection(rejection: SoxRejection) extends Submission
  }

  private[this] val logger = ContextualizedLogger.get(getClass)

  def successMapper(submission: Submission, index: Long, participantId: Ref.ParticipantId): Update =
    submission match {
      case s: Submission.AllocateParty =>
        val party = s.hint.getOrElse(UUID.randomUUID().toString)
        Update.PartyAddedToParticipant(
          party = Ref.Party.assertFromString(party),
          displayName = s.displayName.getOrElse(party),
          participantId = participantId,
          recordTime = Time.Timestamp.now(),
          submissionId = Some(s.submissionId),
        )

      case s: Submission.Config =>
        Update.ConfigurationChanged(
          recordTime = Time.Timestamp.now(),
          submissionId = s.submissionId,
          participantId = participantId,
          newConfiguration = s.config,
        )

      case s: Submission.UploadPackages =>
        Update.PublicPackageUpload(
          archives = s.archives,
          sourceDescription = s.sourceDescription,
          recordTime = Time.Timestamp.now(),
          submissionId = Some(s.submissionId),
        )

      case s: Submission.Transaction =>
        Update.TransactionAccepted(
          optCompletionInfo = Some(s.submitterInfo.toCompletionInfo),
          transactionMeta = s.transactionMeta,
          transaction = s.transaction.asInstanceOf[CommittedTransaction],
          transactionId = Ref.TransactionId.assertFromString(index.toString),
          recordTime = Time.Timestamp.now(),
          divulgedContracts = Nil,
          blindingInfo = None,
        )
    }

  def toOffset(index: Long): Offset = Offset.fromByteArray(Longs.toByteArray(index))

  def toSubmissionResult(
      queueOfferResult: QueueOfferResult
  )(implicit loggingContext: LoggingContext): CompletableFuture[SubmissionResult] =
    CompletableFuture.completedFuture(
      queueOfferResult match {
        case QueueOfferResult.Enqueued => SubmissionResult.Acknowledged
        case QueueOfferResult.Dropped =>
          logger.warn(
            "Buffer overflow: new submission is not added, signalized `Overloaded` for caller."
          )
          SubmissionResult.SynchronousError(
            Status(
              Code.RESOURCE_EXHAUSTED.value
            )
          )
        case QueueOfferResult.Failure(throwable) =>
          logger.error("Error enqueueing new submission.", throwable)
          SubmissionResult.SynchronousError(
            Status(
              Code.INTERNAL.value,
              throwable.getMessage,
            )
          )
        case QueueOfferResult.QueueClosed =>
          logger.error("Error enqueueing new submission: queue is closed.")
          SubmissionResult.SynchronousError(
            Status(
              Code.INTERNAL.value,
              "Queue is closed",
            )
          )
      }
    )
}
