package org.validoc.utils.logging

import java.util.UUID

import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.Eventually
import org.validoc.utils.UtilsWithLoggingSpec
import org.validoc.utils.concurrency.MDCPropagatingExecutionContext
import org.validoc.utils.time.NanoTimeService

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success}
import scala.concurrent.duration._

trait LoggingFixture {

  class LoggingMemoriseForTests extends LoggingMemoriser {

    override val nanoTimeService: NanoTimeService = new NanoTimeService {
      var x: Long = 0

      override def apply(): Long = {
        x += 1
        x
      }
    }
  }

  val e2 = new RuntimeException
}

class LoggingMemoriserTests extends UtilsWithLoggingSpec with LoggingFixture {

  "LoggingMemoriser for non future blocks" should "return the result of the trace" in {
    val memoriser = new LoggingMemoriseForTests

    val LoggingReport(result, records) = memoriser.traceNow("result")
    result shouldBe Success("result")
    records shouldBe LoggingRecords(Vector())
  }

  it should "have a result of Failure(exception) if an exception is thrown" in {
    val memoriser = new LoggingMemoriseForTests
    println(" a result of Failure(exception) if an exception is thrown")
    val LoggingReport(result, records) = memoriser.traceNow {
      memoriser.memorise(Debug, "msg1", null)
      throw e2
    }
    result shouldBe Failure(e2)
    records shouldBe LoggingRecords(Vector(LoggingRecord(1, Debug, "msg1", None)))
  }

  it should "allow memorise to be called after the block is over (requires cleaning the traceid from MDC)" in {
    val memoriser = new LoggingMemoriseForTests
    memoriser.memorise(Debug, "shouldNotBeInResult: occured before trace block", null)
    val LoggingReport(result, records) = memoriser.traceNow {
      memoriser.memorise(Trace, "msg2", e2)
      "result"
    }
    memoriser.memorise(Debug, "shouldNot throw exception", null)

  }

  it should "return the record of the memorize calls that took place in the block" in {
    val memoriser = new LoggingMemoriseForTests
    memoriser.memorise(Debug, "shouldNotBeInResult: occured before trace block", null)
    val LoggingReport(result, records) = memoriser.traceNow {
      memoriser.memorise(Debug, "msg1", null)
      memoriser.memorise(Trace, "msg2", e2)
      "result"
    }
    result shouldBe Success("result")
    records shouldBe LoggingRecords(Vector(
      LoggingRecord(1, Debug, "msg1", None),
      LoggingRecord(2, Trace, "msg2", Some(e2))
    ))
  }
}


class LoggingMemoriserSecretTests extends UtilsWithLoggingSpec with Logging {
  "LoggingMemoriser" should "replace 'secrets' that are being logged" in {
    val secret = UUID.randomUUID.toString

    LoggingMemoriser.registerSecret(secret, _ => "<sec1>")

    val LoggingReport(result, records) = LoggingMemoriser.traceNow {
      debug(s"msg1_${secret}_stuff")
      "result"
    }
    result shouldBe Success("result")
    val LoggingRecords(Vector(LoggingRecord(time, level, msg, _)), Vector()) = records
    msg shouldBe "msg1_<sec1>_stuff"
  }
}

class LoggingMemoriserFutureTests extends UtilsWithLoggingSpec with LoggingFixture with Eventually with BeforeAndAfterEach with Logging {

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearMdc
  }

  override def afterEach(): Unit = {
    super.afterEach()
    clearMdc
  }

  "LoggingMemoriser for future blocks" should "return the result of the trace" in {
    val memoriser = new LoggingMemoriseForTests
    val LoggingReport(result, records) = Await.result(memoriser.traceFuture(Future("result")), 5 seconds)
    result shouldBe Success("result")
    records shouldBe LoggingRecords(Vector())
  }

  it should "have a result of Failure(exception) if an exception is thrown, and keep track of the actual data" in {
    val memoriser = new LoggingMemoriseForTests
    val LoggingReport(result, records) = Await.result(memoriser.traceFuture(Future {
      memoriser.memorise(Debug, "msg1", null);
      throw e2
    }), 5 seconds
    )
    result shouldBe Failure(e2)
    records shouldBe LoggingRecords(Vector(LoggingRecord(1, Debug, "msg1", None)))
  }

}