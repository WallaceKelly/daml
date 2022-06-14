// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.platform.store.dao.events

import java.sql.Connection

import akka.stream.Attributes
import akka.stream.scaladsl.Source
import akka.{Done, NotUsed}
import com.daml.error.DamlContextualizedErrorLogger
import com.daml.ledger.api.TraceIdentifiers
import com.daml.ledger.api.v1.active_contracts_service.GetActiveContractsResponse
import com.daml.ledger.api.v1.event.Event
import com.daml.ledger.api.v1.transaction.TreeEvent
import com.daml.ledger.api.v1.transaction_service.{
  GetFlatTransactionResponse,
  GetTransactionResponse,
  GetTransactionTreesResponse,
  GetTransactionsResponse,
}
import com.daml.ledger.offset.Offset
import com.daml.lf.data.Ref
import com.daml.logging.{ContextualizedLogger, LoggingContext}
import com.daml.metrics._
import com.daml.nameof.NameOf.qualifiedNameOfCurrentFunc
import com.daml.platform._
import com.daml.platform.ApiOffset
import com.daml.platform.store.dao.{
  DbDispatcher,
  LedgerDaoTransactionsReader,
  PaginatingAsyncStream,
}
import com.daml.platform.store.backend.{ContractStorageBackend, EventStorageBackend}
import com.daml.platform.store.dao.events.EventsTable.TransactionConversions
import com.daml.platform.store.interfaces.TransactionLogUpdate
import com.daml.platform.store.utils.{ConcurrencyLimiter, QueueBasedConcurrencyLimiter, Telemetry}
import com.daml.telemetry
import com.daml.telemetry.{SpanAttribute, Spans}
import com.daml.metrics.InstrumentedGraph._
import com.daml.platform.configuration.IndexServiceConfig
import com.daml.platform.indexer.parallel.BatchN
import com.daml.platform.store.backend.EventStorageBackend.Entry
import com.daml.platform.store.dao.PaginatingAsyncStream.IdPaginationState
import com.daml.platform.store.dao.events.FilterTableACSReader.{
  Filter,
  IdQueryConfiguration,
  statefulDeduplicate,
}
import io.opentelemetry.api.trace.Span

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/** @param dispatcher Executes the queries prepared by this object
  * @param executionContext Runs transformations on data fetched from the database, including Daml-LF value deserialization
  * @param pageSize The number of events to fetch at a time the database when serving streaming calls
  * @param eventProcessingParallelism The parallelism for loading and decoding state events
  * @param lfValueTranslation The delegate in charge of translating serialized Daml-LF values
  * @see [[PaginatingAsyncStream]]
  */
private[dao] final class TransactionsReader(
    dispatcher: DbDispatcher,
    queryNonPruned: QueryNonPruned,
    eventStorageBackend: EventStorageBackend,
    contractStorageBackend: ContractStorageBackend,
    pageSize: Int,
    eventProcessingParallelism: Int,
    metrics: Metrics,
    lfValueTranslation: LfValueTranslation,
    acsReader: ACSReader,
    idFetchingGlobalLimiter: ConcurrencyLimiter,
    eventFetchingGlobalLimiter: ConcurrencyLimiter,
)(implicit executionContext: ExecutionContext)
    extends LedgerDaoTransactionsReader {

  private val dbMetrics = metrics.daml.index.db
  private val eventSeqIdReader =
    new EventsRange.EventSeqIdReader(eventStorageBackend.maxEventSequentialIdOfAnObservableEvent)

  private val logger = ContextualizedLogger.get(this.getClass)

  // TransactionReader adds an Akka stream buffer at the end of all streaming queries.
  // This significantly improves the performance of the transaction service.
  //
  // TODO LLP: Remove once getContractStateEvents and getTransactionLogUpdates is removed
  private val outputStreamBufferSize = 128

  private def offsetFor(response: GetTransactionsResponse): Offset =
    ApiOffset.assertFromString(response.transactions.head.offset)

  private def offsetFor(response: GetTransactionTreesResponse): Offset =
    ApiOffset.assertFromString(response.transactions.head.offset)

  private def deserializeEvent[E](verbose: Boolean)(entry: EventStorageBackend.Entry[Raw[E]])(
      implicit loggingContext: LoggingContext
  ): Future[E] =
    entry.event.applyDeserialization(lfValueTranslation, verbose)

  private def deserializeEntry[E](verbose: Boolean)(
      entry: EventStorageBackend.Entry[Raw[E]]
  )(implicit loggingContext: LoggingContext): Future[EventStorageBackend.Entry[E]] =
    deserializeEvent(verbose)(entry).map(event => entry.copy(event = event))

  override def getFlatTransactions(
      startExclusive: Offset,
      endInclusive: Offset,
      filter: FilterRelation,
      verbose: Boolean,
  )(implicit loggingContext: LoggingContext): Source[(Offset, GetTransactionsResponse), NotUsed] = {
    val futureSource = getEventSeqIdRange(startExclusive, endInclusive)
      .map(queryRange => doGetFlatTransactions(queryRange, filter, verbose))
    Source
      .futureSource(futureSource)
      .mapMaterializedValue((_: Future[NotUsed]) => NotUsed)
  }

  def doGetFlatTransactions(
      queryRange: EventsRange[(Offset, Long)],
      filter: FilterRelation,
      verbose: Boolean,
  )(implicit loggingContext: LoggingContext): Source[(Offset, GetTransactionsResponse), NotUsed] = {
    val (firstOffset, firstEventSequentialId): (Offset, Long) = queryRange.startExclusive
    val (lastOffset, lastEventSequentialId): (Offset, Long) = queryRange.endInclusive

    val span: Span =
      Telemetry.Transactions.createSpan(firstOffset, lastOffset)(qualifiedNameOfCurrentFunc)
    logger.debug(s"getFlatTransactions($firstOffset, $lastOffset, $filter, $verbose)")

    // TODO pbatko: configure it upstream
    val idPageSize = IndexServiceConfig.DefaultAcsIdPageSize
    val idPageBufferSize = IndexServiceConfig.DefaultAcsIdPageBufferSize
    val idPageWorkingMemoryBytes = IndexServiceConfig.DefaultAcsIdPageWorkingMemoryBytes

    // NOTE: These must be powers of 2
    val consumingEventsFetchingParallelism_mapAsync = 2
    val createEventsFetchingParallelism_mapAsync = 2

    val idFetchingParallelism = 4
    val eventIdFetchingLimiter_createEvents = new QueueBasedConcurrencyLimiter(
      idFetchingParallelism,
      executionContext,
      parentO = Some(idFetchingGlobalLimiter),
    )
    val eventIdFetchingLimiter_consumingEvents = new QueueBasedConcurrencyLimiter(
      idFetchingParallelism,
      executionContext,
      parentO = Some(idFetchingGlobalLimiter),
    )

    val eventFetchingLimiter: ConcurrencyLimiter = new QueueBasedConcurrencyLimiter(
      2,
      executionContext,
      parentO = Some(eventFetchingGlobalLimiter),
    )

    val filters: Vector[Filter] = filter.iterator.flatMap {
      case (party, emptyTemplateIds) if emptyTemplateIds.isEmpty => Iterator(Filter(party, None))
      case (party, templateIds) =>
        templateIds.iterator.map(templateId => Filter(party, Some(templateId)))
    }.toVector
    val allFilterParties = filter.keySet

    val idPageConfig = IdQueryConfiguration(
      maxIdPageSize = idPageSize,
      idPageWorkingMemoryBytes = idPageWorkingMemoryBytes,
      filterSize = filters.size,
      idPageBufferSize = idPageBufferSize,
    )

    def streamConsumingIds_stakeholders(filter: Filter): Source[Long, NotUsed] = {
      PaginatingAsyncStream.streamIdsFromSeekPagination(
        pageConfig = idPageConfig,
        pageBufferSize = idPageBufferSize,
        initialStartOffset = firstEventSequentialId,
      )(
        fetchPage = (state: IdPaginationState) => {
          eventIdFetchingLimiter_consumingEvents.execute {
            dispatcher.executeSql(metrics.daml.index.db.getConsumingIds_stakeholdersFilter) {
              connection =>
                eventStorageBackend.fetchIds_consuming_stakeholders(
                  partyFilter = filter.party,
                  templateIdFilter = filter.templateId,
                  startExclusive = state.startOffset,
                  endInclusive = lastEventSequentialId,
                  limit = state.pageSize,
                )(connection)
            }
          }
        }
      )
    }

    def streamCreateIds_stakeholders(filter: Filter): Source[Long, NotUsed] = {
      PaginatingAsyncStream.streamIdsFromSeekPagination(
        pageConfig = idPageConfig,
        pageBufferSize = idPageBufferSize,
        initialStartOffset = firstEventSequentialId,
      )(
        fetchPage = (state: IdPaginationState) => {
          eventIdFetchingLimiter_createEvents.execute {
            dispatcher.executeSql(metrics.daml.index.db.getCreateEventIds_stakeholdersFilter) {
              connection =>
                eventStorageBackend.fetchIds_create_stakeholders(
                  partyFilter = filter.party,
                  templateIdFilter = filter.templateId,
                  startExclusive = state.startOffset,
                  endInclusive = lastEventSequentialId,
                  limit = state.pageSize,
                )(connection)
            }
          }
        }
      )
    }

    def decode(
        rawEvents: Vector[EventStorageBackend.Entry[Raw.FlatEvent]]
    ): Future[Vector[EventStorageBackend.Entry[Event]]] = {
      Timed.future(
        future = Future.traverse(rawEvents)(deserializeEntry(verbose)),
        timer = dbMetrics.getFlatTransactions.translationTimer,
      )
    }

    def fetchConsumingEvents(
        ids: Iterable[Long]
    ): Future[Vector[EventStorageBackend.Entry[Raw.FlatEvent]]] = {
      eventFetchingLimiter
        .execute(
          dispatcher.executeSql(metrics.daml.index.db.getFlatTransactions) { implicit connection =>
            queryNonPruned.executeSql(
              query = eventStorageBackend.fetchFlatConsumingEvents(
                eventSequentialIds = ids,
                allFilterParties = allFilterParties,
              )(connection),
              minOffsetExclusive = firstOffset,
              error = (prunedOffset: Offset) =>
                s"Transactions request from ${firstOffset.toHexString} to ${lastOffset.toHexString} precedes pruned offset ${prunedOffset.toHexString}",
            )
          }
        )
    }

    def fetchCreateEvents(
        ids: Iterable[Long]
    ): Future[Vector[EventStorageBackend.Entry[Raw.FlatEvent]]] = {
      eventFetchingLimiter
        .execute(
          dispatcher.executeSql(metrics.daml.index.db.getFlatTransactions) { implicit connection =>
            queryNonPruned.executeSql(
              query = eventStorageBackend.fetchFlatCreateEvents(
                eventSequentialIds = ids,
                allFilterParties = allFilterParties,
              )(connection),
              minOffsetExclusive = firstOffset,
              error = (prunedOffset: Offset) =>
                s"Transactions request from ${firstOffset.toHexString} to ${lastOffset.toHexString} precedes pruned offset ${prunedOffset.toHexString}",
            )
          }
        )
    }

    import scala.util.chaining._

    val consumingStream: Source[EventStorageBackend.Entry[Raw.FlatEvent], NotUsed] = filters
      .map(streamConsumingIds_stakeholders)
      .pipe(FilterTableACSReader.mergeSort[Long])
      .statefulMapConcat(statefulDeduplicate)
      .via(
        BatchN(
          maxBatchSize = pageSize,
          maxBatchCount = consumingEventsFetchingParallelism_mapAsync + 1,
        )
      )
      .async
      .addAttributes(
        Attributes.inputBuffer(
          initial = consumingEventsFetchingParallelism_mapAsync,
          max = consumingEventsFetchingParallelism_mapAsync,
        )
      )
      .mapAsync(consumingEventsFetchingParallelism_mapAsync)(fetchConsumingEvents)
      .mapConcat(identity)

    val createStream: Source[EventStorageBackend.Entry[Raw.FlatEvent], NotUsed] = filters
      .map(streamCreateIds_stakeholders)
      .pipe(FilterTableACSReader.mergeSort[Long])
      .statefulMapConcat(statefulDeduplicate)
      .via(
        BatchN(
          maxBatchSize = pageSize,
          // TODO pbatko: Why +1 ?
          maxBatchCount = createEventsFetchingParallelism_mapAsync + 1,
        )
      )
      .addAttributes(
        Attributes.inputBuffer(
          initial = createEventsFetchingParallelism_mapAsync,
          max = createEventsFetchingParallelism_mapAsync,
        )
      )
      .mapAsync(createEventsFetchingParallelism_mapAsync)(fetchCreateEvents)
      .mapConcat(identity)

    val allFlatEvents: Source[EventStorageBackend.Entry[Raw.FlatEvent], NotUsed] =
      consumingStream.mergeSorted(createStream)(
        ord = new Ordering[EventStorageBackend.Entry[Raw.FlatEvent]] {
          override def compare(
              x: EventStorageBackend.Entry[Raw.FlatEvent],
              y: EventStorageBackend.Entry[Raw.FlatEvent],
          ): Int = {
            implicitly[Ordering[Long]].compare(x.eventSequentialId, y.eventSequentialId)
          }
        }
      )

    TransactionsReader
      .groupContiguous(allFlatEvents)(by = _.transactionId)
      .mapAsync(eventProcessingParallelism)(decode)
      .mapConcat { events: Vector[EventStorageBackend.Entry[Event]] =>
        val response = TransactionConversions.toGetTransactionsResponse(events)
        response.map(r => offsetFor(r) -> r)
      }
      .wireTap(_ match {
        case (_, response) =>
          response.transactions.foreach(txn =>
            Spans.addEventToSpan(
              telemetry.Event("transaction", TraceIdentifiers.fromTransaction(txn)),
              span,
            )
          )
      })
      .watchTermination()(endSpanOnTermination(span))
  }

  sealed trait PointWiseTransactionFetching {
    type EventT
    type RawEventT <: Raw[EventT]
    type RespT

    protected val dbMetric: DatabaseMetrics

    protected def fetchTransaction(
        firstEventSequentialId: Long,
        lastEventSequentialId: Long,
        requestingParties: Set[Party],
    )(connection: Connection): Vector[EventStorageBackend.Entry[RawEventT]]

    protected def toTransactionResponse(
        events: Vector[Entry[EventT]]
    ): Option[RespT]

    final def lookupTransactionById(
        transactionId: Ref.TransactionId,
        requestingParties: Set[Party],
    )(implicit loggingContext: LoggingContext): Future[Option[RespT]] = {
      val requestingPartiesStrings: Set[String] = requestingParties.toSet[String]
      for {
        // Fetching event seq. id range corresponding to the requested transaction id
        eventSeqIdRangeO <- dispatcher.executeSql(dbMetric)(
          eventStorageBackend.fetchIdsFromTransactionMeta(transactionId = transactionId)
        )
        response <- eventSeqIdRangeO match {
          case Some((firstEventSeqId, lastEventSeqId)) =>
            for {
              // Fetching all events from the event seq. id range
              rawEvents <- dispatcher.executeSql(dbMetric)(
                fetchTransaction(
                  firstEventSequentialId = firstEventSeqId,
                  lastEventSequentialId = lastEventSeqId,
                  requestingParties = requestingParties,
                )
              )
              // Filtering by requesting parties
              filteredRawEvents = rawEvents.filter(
                _.event.witnesses.exists(requestingPartiesStrings)
              )
              // Deserialization of lf values
              deserialized <- Timed.value(
                timer = dbMetric.translationTimer,
                value = Future.traverse(filteredRawEvents)(deserializeEntry(verbose = true)),
              )
            } yield {
              // Conversion to API response type
              toTransactionResponse(deserialized)
            }
          case None => Future.successful[Option[RespT]](None)
        }
      } yield response
    }
  }

  object TreeTransactionFetching extends PointWiseTransactionFetching {

    override type EventT = TreeEvent
    override type RawEventT = Raw.TreeEvent
    override type RespT = GetTransactionResponse

    override val dbMetric: DatabaseMetrics = dbMetrics.lookupTransactionTreeById

    override protected def fetchTransaction(
        firstEventSequentialId: Long,
        lastEventSequentialId: Long,
        requestingParties: Set[Party],
    )(connection: Connection): Vector[EventStorageBackend.Entry[RawEventT]] = {
      eventStorageBackend.fetchTreeTransaction(
        firstEventSequentialId = firstEventSequentialId,
        lastEventSequentialId = lastEventSequentialId,
        requestingParties = requestingParties,
      )(connection)
    }

    override protected def toTransactionResponse(events: Vector[Entry[EventT]]): Option[RespT] = {
      TransactionConversions.toGetTransactionResponse(events)
    }
  }

  object FlatTransactionFetching extends PointWiseTransactionFetching {

    override type EventT = Event
    override type RawEventT = Raw.FlatEvent
    override type RespT = GetFlatTransactionResponse

    override val dbMetric: DatabaseMetrics = dbMetrics.lookupFlatTransactionById

    override protected def fetchTransaction(
        firstEventSequentialId: Long,
        lastEventSequentialId: Long,
        requestingParties: Set[Party],
    )(connection: Connection): Vector[EventStorageBackend.Entry[RawEventT]] = {
      eventStorageBackend.fetchFlatTransaction(
        firstEventSequentialId = firstEventSequentialId,
        lastEventSequentialId = lastEventSequentialId,
        requestingParties = requestingParties,
      )(connection)
    }

    override protected def toTransactionResponse(events: Vector[Entry[EventT]]): Option[RespT] = {
      TransactionConversions.toGetFlatTransactionResponse(events)
    }
  }

  override def lookupFlatTransactionById(
      transactionId: Ref.TransactionId,
      requestingParties: Set[Party],
  )(implicit loggingContext: LoggingContext): Future[Option[GetFlatTransactionResponse]] = {
    FlatTransactionFetching.lookupTransactionById(
      transactionId = transactionId,
      requestingParties = requestingParties,
    )
  }

  override def lookupTransactionTreeById(
      transactionId: Ref.TransactionId,
      requestingParties: Set[Party],
  )(implicit loggingContext: LoggingContext): Future[Option[GetTransactionResponse]] = {
    TreeTransactionFetching.lookupTransactionById(
      transactionId = transactionId,
      requestingParties = requestingParties,
    )
  }

  override def getTransactionTrees(
      startExclusive: Offset,
      endInclusive: Offset,
      requestingParties: Set[Party],
      verbose: Boolean,
  )(implicit
      loggingContext: LoggingContext
  ): Source[(Offset, GetTransactionTreesResponse), NotUsed] = {
    val requestedRangeF: Future[EventsRange[(Offset, Long)]] =
      getEventSeqIdRange(startExclusive, endInclusive)
    val futureSource = requestedRangeF.map(queryRange =>
      doGetTransactionTrees(
        queryRange = queryRange,
        requestingParties = requestingParties,
        verbose = verbose,
      )
    )
    Source
      .futureSource(futureSource)
      .mapMaterializedValue((_: Future[NotUsed]) => NotUsed)
  }

  private def doGetTransactionTrees(
      queryRange: EventsRange[(Offset, Long)],
      requestingParties: Set[Party],
      verbose: Boolean,
  )(implicit
      loggingContext: LoggingContext
  ): Source[(Offset, GetTransactionTreesResponse), NotUsed] = {
    val (firstOffset, firstEventSequentialId): (Offset, Long) = queryRange.startExclusive
    val (lastOffset, lastEventSequentialId): (Offset, Long) = queryRange.endInclusive

    val span =
      Telemetry.Transactions.createSpan(firstOffset, lastOffset)(qualifiedNameOfCurrentFunc)
    logger.debug(
      s"getTransactionTrees($firstOffset, $lastOffset, $requestingParties, $verbose)"
    )

    // TODO pbatko: configure it upstream
    val idPageSize = IndexServiceConfig.DefaultAcsIdPageSize
    val idPageBufferSize = IndexServiceConfig.DefaultAcsIdPageBufferSize
    val idPageWorkingMemoryBytes = IndexServiceConfig.DefaultAcsIdPageWorkingMemoryBytes

    val eventsFetchingAndDecodingParallelism_mapAsync = 2
    val idFetchingLimiter_create =
      new QueueBasedConcurrencyLimiter(8, executionContext, parentO = Some(idFetchingGlobalLimiter))
    val idFetchingLimiter_consuming =
      new QueueBasedConcurrencyLimiter(8, executionContext, parentO = Some(idFetchingGlobalLimiter))
    val idFetchingLimiter_nonConsuming =
      new QueueBasedConcurrencyLimiter(4, executionContext, parentO = Some(idFetchingGlobalLimiter))
    val eventFetchingLimiter: ConcurrencyLimiter = new QueueBasedConcurrencyLimiter(
      2,
      executionContext,
      parentO = Some(eventFetchingGlobalLimiter),
    )

    val filters: Vector[Filter] =
      requestingParties.iterator.map(party => Filter(party, None)).toVector
    val allFilterParties = requestingParties

    val idPageConfig = IdQueryConfiguration(
      maxIdPageSize = idPageSize,
      idPageWorkingMemoryBytes = idPageWorkingMemoryBytes,
      filterSize = filters.size,
      idPageBufferSize = idPageBufferSize,
    )

    def streamConsumingIds_stakeholders(filter: Filter): Source[Long, NotUsed] = {
      PaginatingAsyncStream.streamIdsFromSeekPagination(
        pageConfig = idPageConfig,
        pageBufferSize = idPageBufferSize,
        initialStartOffset = firstEventSequentialId,
      )(
        fetchPage = (state: IdPaginationState) => {
          idFetchingLimiter_consuming.execute {
            dispatcher.executeSql(metrics.daml.index.db.getConsumingIds_stakeholdersFilter) {
              connection =>
                eventStorageBackend.fetchIds_consuming_stakeholders(
                  partyFilter = filter.party,
                  templateIdFilter = filter.templateId,
                  startExclusive = state.startOffset,
                  endInclusive = lastEventSequentialId,
                  limit = state.pageSize,
                )(connection)
            }
          }
        }
      )
    }

    def streamConsumingIds_nonStakeholderInformees(filter: Filter): Source[Long, NotUsed] = {
      PaginatingAsyncStream.streamIdsFromSeekPagination(
        pageConfig = idPageConfig,
        pageBufferSize = idPageBufferSize,
        initialStartOffset = firstEventSequentialId,
      )(
        fetchPage = (state: IdPaginationState) => {
          idFetchingLimiter_consuming.execute {
            dispatcher.executeSql(
              metrics.daml.index.db.getConsumingIds_nonStakeholderInformeesFilter
            ) { connection =>
              eventStorageBackend.fetchIds_consuming_nonStakeholderInformees(
                partyFilter = filter.party,
                startExclusive = state.startOffset,
                endInclusive = lastEventSequentialId,
                limit = state.pageSize,
              )(connection)
            }
          }
        }
      )
    }

    def streamCreateIds_stakeholders(filter: Filter): Source[Long, NotUsed] = {
      PaginatingAsyncStream.streamIdsFromSeekPagination(
        pageConfig = idPageConfig,
        pageBufferSize = idPageBufferSize,
        initialStartOffset = firstEventSequentialId,
      )(
        fetchPage = (state: IdPaginationState) => {
          idFetchingLimiter_create.execute {
            dispatcher.executeSql(metrics.daml.index.db.getCreateEventIds_stakeholdersFilter) {
              connection =>
                eventStorageBackend.fetchIds_create_stakeholders(
                  partyFilter = filter.party,
                  templateIdFilter = filter.templateId,
                  startExclusive = state.startOffset,
                  endInclusive = lastEventSequentialId,
                  limit = state.pageSize,
                )(connection)
            }
          }
        }
      )
    }

    def streamCreateIds_nonStakeholderInformees(filter: Filter): Source[Long, NotUsed] = {
      PaginatingAsyncStream.streamIdsFromSeekPagination(
        pageConfig = idPageConfig,
        pageBufferSize = idPageBufferSize,
        initialStartOffset = firstEventSequentialId,
      )(
        fetchPage = (state: IdPaginationState) => {
          idFetchingLimiter_create.execute {
            dispatcher.executeSql(
              metrics.daml.index.db.getCreateEventIds_nonStakeholderInformeesFilter
            ) { connection =>
              eventStorageBackend.fetchIds_create_nonStakeholderInformees(
                partyFilter = filter.party,
                startExclusive = state.startOffset,
                endInclusive = lastEventSequentialId,
                limit = state.pageSize,
              )(connection)
            }
          }
        }
      )
    }

    def streamNonConsumingIds_informees(filter: Filter): Source[Long, NotUsed] = {
      PaginatingAsyncStream.streamIdsFromSeekPagination(
        pageConfig = idPageConfig,
        pageBufferSize = idPageBufferSize,
        initialStartOffset = firstEventSequentialId,
      )(
        fetchPage = (state: IdPaginationState) => {
          idFetchingLimiter_nonConsuming.execute {
            dispatcher.executeSql(metrics.daml.index.db.getNonConsumingEventIds_informeesFilter) {
              connection =>
                eventStorageBackend.fetchIds_nonConsuming_informees(
                  partyFilter = filter.party,
                  startExclusive = state.startOffset,
                  endInclusive = lastEventSequentialId,
                  limit = state.pageSize,
                )(connection)
            }
          }
        }
      )
    }

    def timedDeserialize(
        rawEvents: Vector[EventStorageBackend.Entry[Raw.TreeEvent]]
    ): Future[Vector[EventStorageBackend.Entry[TreeEvent]]] = {
      Timed.future(
        future = Future.traverse(rawEvents)(deserializeEntry(verbose)),
        timer = dbMetrics.getFlatTransactions.translationTimer,
      )
    }

    def fetchConsumingEvents(
        ids: Iterable[Long]
    ): Future[Vector[EventStorageBackend.Entry[Raw.TreeEvent]]] = {
      eventFetchingLimiter
        .execute(
          dispatcher.executeSql(metrics.daml.index.db.getTransactionTrees) { implicit connection =>
            queryNonPruned.executeSql(
              query = eventStorageBackend.fetchTreeConsumingEvents(
                eventSequentialIds = ids,
                allFilterParties = allFilterParties,
              )(connection),
              minOffsetExclusive = firstOffset,
              error = (prunedOffset: Offset) =>
                s"Transactions request from ${firstOffset.toHexString} to ${lastOffset.toHexString} precedes pruned offset ${prunedOffset.toHexString}",
            )
          }
        )
    }

    def fetchCreateEvents(
        ids: Iterable[Long]
    ): Future[Vector[EventStorageBackend.Entry[Raw.TreeEvent]]] = {
      eventFetchingLimiter
        .execute(
          dispatcher.executeSql(metrics.daml.index.db.getTransactionTrees) { implicit connection =>
            queryNonPruned.executeSql(
              query = eventStorageBackend.fetchTreeCreateEvents(
                eventSequentialIds = ids,
                allFilterParties = allFilterParties,
              )(connection),
              minOffsetExclusive = firstOffset,
              error = (prunedOffset: Offset) =>
                s"Transactions request from ${firstOffset.toHexString} to ${lastOffset.toHexString} precedes pruned offset ${prunedOffset.toHexString}",
            )
          }
        )
    }

    def fetchNonConsumingEvents(
        ids: Iterable[Long]
    ): Future[Vector[EventStorageBackend.Entry[Raw.TreeEvent]]] = {
      eventFetchingLimiter
        .execute(
          dispatcher.executeSql(metrics.daml.index.db.getTransactionTrees) { implicit connection =>
            queryNonPruned.executeSql(
              query = eventStorageBackend.fetchTreeNonConsumingEvents(
                eventSequentialIds = ids,
                allFilterParties = allFilterParties,
              )(connection),
              minOffsetExclusive = firstOffset,
              error = (prunedOffset: Offset) =>
                s"Transactions request from ${firstOffset.toHexString} to ${lastOffset.toHexString} precedes pruned offset ${prunedOffset.toHexString}",
            )
          }
        )
    }

    import scala.util.chaining._

    val consumingStream: Source[EventStorageBackend.Entry[Raw.TreeEvent], NotUsed] = {
      filters.map(streamConsumingIds_stakeholders) ++ filters.map(
        streamConsumingIds_nonStakeholderInformees
      )
    }
      .pipe(FilterTableACSReader.mergeSort[Long])
      .statefulMapConcat(statefulDeduplicate)
      .via(
        BatchN(
          maxBatchSize = pageSize,
          maxBatchCount = eventsFetchingAndDecodingParallelism_mapAsync + 1,
        )
      )
      .async
      .addAttributes(
        Attributes.inputBuffer(
          initial = eventsFetchingAndDecodingParallelism_mapAsync,
          max = eventsFetchingAndDecodingParallelism_mapAsync,
        )
      )
      .mapAsync(eventsFetchingAndDecodingParallelism_mapAsync)(fetchConsumingEvents)
      .mapConcat(identity)

    val createStream: Source[EventStorageBackend.Entry[Raw.TreeEvent], NotUsed] = {
      filters.map(streamCreateIds_stakeholders) ++ filters.map(
        streamCreateIds_nonStakeholderInformees
      )
    }
      .pipe(FilterTableACSReader.mergeSort[Long])
      .statefulMapConcat(statefulDeduplicate)
      .via(
        BatchN(
          maxBatchSize = pageSize,
          maxBatchCount = eventsFetchingAndDecodingParallelism_mapAsync + 1,
        )
      )
      .async
      .addAttributes(
        Attributes.inputBuffer(
          initial = eventsFetchingAndDecodingParallelism_mapAsync,
          max = eventsFetchingAndDecodingParallelism_mapAsync,
        )
      )
      .mapAsync(eventsFetchingAndDecodingParallelism_mapAsync)(fetchCreateEvents)
      .mapConcat(identity)

    val nonConsumingStream: Source[EventStorageBackend.Entry[Raw.TreeEvent], NotUsed] = filters
      .map(streamNonConsumingIds_informees)
      .pipe(FilterTableACSReader.mergeSort[Long])
      .statefulMapConcat(statefulDeduplicate)
      .via(
        BatchN(
          maxBatchSize = pageSize,
          maxBatchCount = eventsFetchingAndDecodingParallelism_mapAsync + 1,
        )
      )
      .addAttributes(
        Attributes.inputBuffer(
          initial = eventsFetchingAndDecodingParallelism_mapAsync,
          max = eventsFetchingAndDecodingParallelism_mapAsync,
        )
      )
      .mapAsync(eventsFetchingAndDecodingParallelism_mapAsync)(fetchNonConsumingEvents)
      .mapConcat(identity)

    val ordering = new Ordering[EventStorageBackend.Entry[Raw.TreeEvent]] {
      override def compare(
          x: EventStorageBackend.Entry[Raw.TreeEvent],
          y: EventStorageBackend.Entry[Raw.TreeEvent],
      ): Int = {
        implicitly[Ordering[Long]].compare(x.eventSequentialId, y.eventSequentialId)
      }
    }
    val allEvents: Source[EventStorageBackend.Entry[Raw.TreeEvent], NotUsed] =
      consumingStream
        .mergeSorted(createStream)(ord = ordering)
        .mergeSorted(nonConsumingStream)(ord = ordering)

    TransactionsReader
      .groupContiguous(allEvents)(by = _.transactionId)
      .mapAsync(eventProcessingParallelism)(timedDeserialize)
      .mapConcat { events =>
        val response = TransactionConversions.toGetTransactionTreesResponse(events)
        response.map(r => offsetFor(r) -> r)
      }
      .wireTap(_ match {
        case (_, response) =>
          response.transactions.foreach(txn =>
            Spans.addEventToSpan(
              telemetry.Event("transaction", TraceIdentifiers.fromTransactionTree(txn)),
              span,
            )
          )
      })
      .watchTermination()(endSpanOnTermination(span))
  }

  override def getTransactionLogUpdates(
      startExclusive: (Offset, Long),
      endInclusive: (Offset, Long),
  )(implicit
      loggingContext: LoggingContext
  ): Source[((Offset, Long), TransactionLogUpdate), NotUsed] = {
    val endMarker = Source.single(
      endInclusive -> TransactionLogUpdate.LedgerEndMarker(
        eventOffset = endInclusive._1,
        eventSequentialId = endInclusive._2,
      )
    )

    val eventsSource = Source
      .fromIterator(() =>
        TransactionsReader
          .splitRange(
            startExclusive._2,
            endInclusive._2,
            eventProcessingParallelism,
            pageSize,
          )
          .iterator
      )
      .map { range =>
        metrics.daml.services.index.getTransactionLogUpdatesChunkSize.update(
          range.endInclusive - range.startExclusive
        )
        range
      }
      // Dispatch database fetches in parallel
      .mapAsync(eventProcessingParallelism) { range =>
        dispatcher.executeSql(dbMetrics.getTransactionLogUpdates) { implicit conn =>
          queryNonPruned.executeSql(
            query = eventStorageBackend.rawEvents(
              startExclusive = range.startExclusive,
              endInclusive = range.endInclusive,
            )(conn),
            minOffsetExclusive = startExclusive._1,
            error = pruned =>
              s"Transaction log updates request after ${startExclusive._1.toHexString} precedes pruned offset ${pruned.toHexString}",
          )
        }
      }
      .async
      // Decode transaction log updates in parallel
      .mapAsync(eventProcessingParallelism) { raw =>
        Timed.future(
          metrics.daml.index.decodeTransactionLogUpdate,
          Future(raw.map(TransactionLogUpdatesReader.toTransactionEvent)),
        )
      }
      .mapConcat(identity)

    TransactionsReader
      .groupContiguous(eventsSource)(by = _.transactionId)
      .map { v =>
        val tx = toTransaction(v)
        (tx.offset, tx.events.last.eventSequentialId) -> tx
      }
      .mapMaterializedValue(_ => NotUsed)
      .buffered(metrics.daml.index.transactionLogUpdatesBufferSize, outputStreamBufferSize)
      .concat(endMarker)
  }

  private def toTransaction(
      events: Vector[TransactionLogUpdate.Event]
  ): TransactionLogUpdate.Transaction = {
    val first = events.head
    TransactionLogUpdate.Transaction(
      transactionId = first.transactionId,
      workflowId = first.workflowId,
      effectiveAt = first.ledgerEffectiveTime,
      offset = first.eventOffset,
      events = events,
    )
  }

  override def getActiveContracts(
      activeAt: Offset,
      filter: FilterRelation,
      verbose: Boolean,
  )(implicit loggingContext: LoggingContext): Source[GetActiveContractsResponse, NotUsed] = {
    val contextualizedErrorLogger = new DamlContextualizedErrorLogger(logger, loggingContext, None)
    val span =
      Telemetry.Transactions.createSpan(activeAt)(qualifiedNameOfCurrentFunc)
    logger.debug(s"getActiveContracts($activeAt, $filter, $verbose)")

    Source
      .futureSource(
        getAcsEventSeqIdRange(activeAt)
          .map(requestedRange => acsReader.acsStream(filter, requestedRange.endInclusive))
      )
      .mapAsync(eventProcessingParallelism) {
        rawResult: Vector[EventStorageBackend.Entry[Raw.FlatEvent]] =>
          Timed.future(
            future = Future.traverse(rawResult)(
              deserializeEntry(verbose)
            ),
            timer = dbMetrics.getActiveContracts.translationTimer,
          )
      }
      .mapConcat(TransactionConversions.toGetActiveContractsResponse(_)(contextualizedErrorLogger))
      .wireTap(response => {
        Spans.addEventToSpan(
          telemetry.Event("contract", Map((SpanAttribute.Offset, response.offset))),
          span,
        )
      })
      .mapMaterializedValue(_ => NotUsed)
      .watchTermination()(endSpanOnTermination(span))
  }

  override def getContractStateEvents(startExclusive: (Offset, Long), endInclusive: (Offset, Long))(
      implicit loggingContext: LoggingContext
  ): Source[((Offset, Long), Vector[ContractStateEvent]), NotUsed] = {

    val endMarker = Source.single(
      endInclusive -> Vector(
        ContractStateEvent.LedgerEndMarker(
          eventOffset = endInclusive._1,
          eventSequentialId = endInclusive._2,
        )
      )
    )

    val contractStateEventsSource = Source
      .fromIterator(() =>
        TransactionsReader
          .splitRange(
            startExclusive._2,
            endInclusive._2,
            eventProcessingParallelism,
            pageSize,
          )
          .iterator
      )
      .map { range =>
        metrics.daml.services.index.getContractStateEventsChunkSize.update(
          range.endInclusive - range.startExclusive
        )
        range
      }
      // Dispatch database fetches in parallel
      .mapAsync(eventProcessingParallelism) { range =>
        dispatcher.executeSql(dbMetrics.getContractStateEvents) { implicit conn =>
          queryNonPruned.executeSql(
            contractStorageBackend
              .contractStateEvents(range.startExclusive, range.endInclusive)(conn),
            startExclusive._1,
            pruned =>
              s"Contract state events request from ${range.startExclusive.toHexString} to ${range.endInclusive.toHexString} precedes pruned offset ${pruned.toHexString}",
          )
        }
      }
      .mapConcat(identity)
      .async
      .mapAsync(eventProcessingParallelism) { raw =>
        Timed.future(
          metrics.daml.index.decodeStateEvent,
          Future(ContractStateEventsReader.toContractStateEvent(raw, lfValueTranslation)),
        )
      }
      .map(event => (event.eventOffset, event.eventSequentialId) -> event)

    TransactionsReader
      .groupContiguous(contractStateEventsSource)(by = { case ((offset, _), _) => offset })
      .map { v =>
        val offset = v.head._1
        offset -> v.map(_._2)
      }
      .buffered(metrics.daml.index.contractStateEventsBufferSize, outputStreamBufferSize)
      .concat(endMarker)
  }

  private def getAcsEventSeqIdRange(activeAt: Offset)(implicit
      loggingContext: LoggingContext
  ): Future[EventsRange[(Offset, Long)]] =
    dispatcher
      .executeSql(dbMetrics.getAcsEventSeqIdRange)(implicit connection =>
        queryNonPruned.executeSql(
          eventSeqIdReader.readEventSeqIdRange(activeAt)(connection),
          activeAt,
          pruned =>
            s"Active contracts request after ${activeAt.toHexString} precedes pruned offset ${pruned.toHexString}",
        )
      )
      .map { x =>
        EventsRange(
          startExclusive = (Offset.beforeBegin, 0),
          endInclusive = (activeAt, x.endInclusive),
        )
      }

  private def getEventSeqIdRange(
      startExclusive: Offset,
      endInclusive: Offset,
  )(implicit loggingContext: LoggingContext): Future[EventsRange[(Offset, Long)]] =
    dispatcher
      .executeSql(dbMetrics.getEventSeqIdRange)(implicit connection =>
        queryNonPruned.executeSql(
          eventSeqIdReader.readEventSeqIdRange(EventsRange(startExclusive, endInclusive))(
            connection
          ),
          startExclusive,
          pruned =>
            s"Transactions request from ${startExclusive.toHexString} to ${endInclusive.toHexString} precedes pruned offset ${pruned.toHexString}",
        )
      )
      .map(x =>
        EventsRange(
          startExclusive = (startExclusive, x.startExclusive),
          endInclusive = (endInclusive, x.endInclusive),
        )
      )

  private def endSpanOnTermination[Mat, Out](span: Span)(mat: Mat, done: Future[Done]): Mat = {
    done.onComplete {
      case Failure(exception) =>
        span.recordException(exception)
        span.end()
      case Success(_) =>
        span.end()
    }
    mat
  }
}

private[dao] object TransactionsReader {

  /** Splits a range of events into equally-sized sub-ranges.
    *
    * @param startExclusive The start exclusive of the range to be split
    * @param endInclusive The end inclusive of the range to be split
    * @param numberOfChunks The number of desired target sub-ranges
    * @param maxChunkSize Maximum sub-range size.
    * @return The ordered sequence of sub-ranges with non-overlapping bounds.
    */
  private[dao] def splitRange(
      startExclusive: Long,
      endInclusive: Long,
      numberOfChunks: Int,
      maxChunkSize: Int,
  ): Vector[EventsRange[Long]] = {
    require(
      maxChunkSize > 0,
      s"Maximum chunk size must be strictly positive, but was $maxChunkSize",
    )
    require(
      numberOfChunks > 0,
      s"You can only split a range in a strictly positive number of chunks ($numberOfChunks)",
    )

    val rangeSize = endInclusive - startExclusive
    require(
      rangeSize >= 0,
      s"Range size should be positive but got bounds ($startExclusive, $endInclusive]",
    )

    val minChunkSize = math.max(1, maxChunkSize / 10)

    if (rangeSize == 0L)
      Vector.empty
    else {
      val targetChunkSize = rangeSize / numberOfChunks.toLong

      if (targetChunkSize < minChunkSize.toLong) {
        val effectiveNumberOfChunks = rangeSize / minChunkSize.toLong

        if (effectiveNumberOfChunks <= 1) {
          Vector(EventsRange(startExclusive, endInclusive))
        } else {
          splitRangeUnsafe(startExclusive, rangeSize, effectiveNumberOfChunks.toInt)
        }
      } else {
        val effectiveNumberOfChunks =
          Math.max(
            numberOfChunks,
            Math.ceil(rangeSize.toDouble / maxChunkSize.toDouble).toInt,
          )
        splitRangeUnsafe(startExclusive, rangeSize, effectiveNumberOfChunks)
      }
    }
  }

  private def splitRangeUnsafe(
      startExclusive: Long,
      rangeSize: Long,
      numberOfChunks: Int,
  ): Vector[EventsRange[Long]] = {
    val minStep = rangeSize / numberOfChunks.toLong

    val remainder = rangeSize - minStep * numberOfChunks.toLong

    (0 until numberOfChunks)
      .foldLeft(
        (startExclusive, remainder, Vector.empty[EventsRange[Long]])
      ) {
        case ((lastStartExclusive, 0L, ranges), _) =>
          val endInclusiveChunk = lastStartExclusive + minStep
          (endInclusiveChunk, 0L, ranges :+ EventsRange(lastStartExclusive, endInclusiveChunk))

        case ((lastStartExclusive, remainder, ranges), _) =>
          val endInclusiveChunk = lastStartExclusive + minStep + 1L
          val rangesResult = ranges :+ EventsRange(lastStartExclusive, endInclusiveChunk)
          (endInclusiveChunk, remainder - 1L, rangesResult)
      }
      ._3
  }

  /** Groups together items of type [[A]] that share an attribute [[K]] over a
    * contiguous stretch of the input [[Source]]. Well suited to perform group-by
    * operations of streams where [[K]] attributes are either sorted or at least
    * show up in blocks.
    *
    * Implementation detail: this method _must_ use concatSubstreams instead of
    * mergeSubstreams to prevent the substreams to be processed in parallel,
    * potentially causing the outputs to be delivered in a different order.
    *
    * Docs: https://doc.akka.io/docs/akka/2.6.10/stream/stream-substream.html#groupby
    */
  def groupContiguous[A, K, Mat](
      source: Source[A, Mat]
  )(by: A => K): Source[Vector[A], Mat] =
    source
      .statefulMapConcat(() => {
        var previousSegmentKey: Option[K] = None
        entry => {
          val keyForEntry = by(entry)
          val entryWithSplit = entry -> !previousSegmentKey.contains(keyForEntry)
          previousSegmentKey = Some(keyForEntry)
          List(entryWithSplit)
        }
      })
      .splitWhen(_._2)
      .map(_._1)
      .fold(Vector.empty[A])(_ :+ _)
      .concatSubstreams
}
