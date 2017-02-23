package org.validoc.utils.logging

import org.validoc.utils.UtilsSpec


class LoggingTests extends UtilsSpec with Logging {

  val runtimeException = new RuntimeException

  "The logging methods" should "all result in a logging record" in {
    val LoggingReport(_, LoggingRecords(records, spans)) = LoggingMemoriser.traceNow {
      trace("trace")
      debug("debug")
      debug("debugE", runtimeException)
      error("error")
      error("errorE", runtimeException)
      info("info")
    }
    records.map(_.msg) shouldBe Vector(
      "trace",
      "debug",
      "debugE",
      "error",
      "errorE",
      "info"
    )
  }

}
