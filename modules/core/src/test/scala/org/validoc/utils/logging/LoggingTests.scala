package org.validoc.utils.logging

import org.validoc.utils.UtilsSpec

import scala.util.{Failure, Success}


class LoggingTests extends UtilsSpec with Logging {

  val runtimeException = new RuntimeException

  "The logging methods" should "all result in a logging record" in {
    val LoggingReport(Success("hello"), LoggingRecords(records, spans)) = LoggingMemoriser.traceNow {
      trace("trace")
      debug("debug")
      debug("debugE", runtimeException)
      error("error")
      error("errorE", runtimeException)
      info("info")
      "hello"
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


  it should "progagate exceptions" in {
    val LoggingReport(Failure(actualException), LoggingRecords(records, spans)) = LoggingMemoriser.traceNow {
      trace("trace")
      debug("debug")
      debug("debugE", runtimeException)
      error("error")
      error("errorE", runtimeException)
      info("info")
      throw runtimeException
    }
    actualException shouldBe runtimeException
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
