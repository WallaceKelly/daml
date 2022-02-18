// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.platform.index

import akka.NotUsed
import akka.stream._
import akka.stream.scaladsl.Source
import com.daml.logging.LoggingContext
import com.daml.platform.store.appendonlydao.LedgerReadDao
import com.daml.platform.store.backend.ParameterStorageBackend.LedgerEnd

import scala.concurrent.Future
import scala.concurrent.duration._

/** Periodically polls the ledger end from the [[LedgerReadDao]]
  * and provides it to a consuming function.
  */
private[index] object LedgerEndPoller {
  def apply(
      ledgerReadDao: LedgerReadDao,
      consume: LedgerEnd => Future[Unit],
  )(implicit
      mat: Materializer,
      loggingContext: LoggingContext,
  ): RestartableManagedStream[LedgerEnd] =
    RestartableManagedStream.withAsyncConsumer(
      name = "ledger end poller",
      streamBuilder = () =>
        Source
          .tick(0.millis, 100.millis, ())
          .mapMaterializedValue(_ => NotUsed)
          .mapAsync(parallelism = 1)(_ => ledgerReadDao.lookupLedgerEnd()),
      restartSettings =
        RestartSettings(minBackoff = 1.second, maxBackoff = 10.seconds, randomFactor = 0.2),
      consumeAsync = consume,
      teardown = System.exit,
    )
}
