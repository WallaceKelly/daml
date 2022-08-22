// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.ledger.api.testtool.infrastructure

import com.daml.ledger.test.TestDar
import com.google.protobuf.ByteString

object Dars {

  // The list of all DAR packages that are bundled with this binary.
  // The TestDar object is generated by Bazel.
  val resources: List[String] = TestDar.paths

  // `DoNotLoadAtStartupResources` are not uploaded during the startup as being uploaded
  // later in the conformance test.
  // TODO when Interface Subscriptions land stable version,
  // this can be updated to import classes instead of Strings
  // import com.daml.ledger.test.{Carbonv2TestDar, Carbonv1TestDar}
  private val DoNotLoadAtStartupResources: Set[String] = Set(
    "ledger/test-common/carbonv1-tests-1.dev.dar",
    "ledger/test-common/carbonv2-tests-1.dev.dar",
  )

  val startupResources: List[String] = resources.filterNot(DoNotLoadAtStartupResources.contains)

  def read(name: String): ByteString =
    ByteString.readFrom(getClass.getClassLoader.getResourceAsStream(name))

}
