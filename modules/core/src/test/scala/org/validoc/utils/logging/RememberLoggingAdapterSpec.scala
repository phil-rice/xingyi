package org.validoc.utils.logging

import org.scalatest.Matchers
import org.validoc.utils.UtilsSpec
import org.validoc.utils.local.LocalOpsPimper
import org.validoc.utils.time.{MockTimeService, NanoTimeService}

import scala.concurrent.Future
import scala.language.higherKinds

trait LoggingAdapterFixture extends Matchers {
  val loggingRunTimeError = new RuntimeException("someMessageForLogging")
  def recordMessage: LoggingAdapter => Unit = { adapter =>
    adapter.debug("sender1")("msgDebug")
    adapter.info("sender2")("info")
    adapter.trace("sender3")("trace")
    adapter.error("sender4")("error", loggingRunTimeError)
  }

  def assertLogsAsExpected(list: List[LoggingRecord], startTime: Long = 0): Unit = {
    list shouldBe List(
      LoggingRecord(100 + startTime, "DEBUG", "sender1/msgDebug", None),
      LoggingRecord(200 + startTime, "INFO", "sender2/info", None),
      LoggingRecord(300 + startTime, "TRACE", "sender3/trace", None),
      LoggingRecord(400 + startTime, "ERROR", "sender4/error", Some(loggingRunTimeError)))
  }

  def useRememberAdapter(block: LoggingAdapter => Unit): RememberLoggingAdapter = {
    implicit val nanoTimeService = new MockTimeService
    val rememberLoggingAdapter = new RememberLoggingAdapter
    block(rememberLoggingAdapter)
    rememberLoggingAdapter
  }

  def useRememberOrNormalAdapter(block: LoggingAdapter => Unit): RememberLoggingAdapter = {
    implicit val nanoTimeService = new MockTimeService
    val normalLoggingAdapter = new RememberLoggingAdapter
    val rememberOrNormalAdapter = new RememberOrNormalAdapter[Future](normalLoggingAdapter)
    block(rememberOrNormalAdapter)
    normalLoggingAdapter
  }

}

class RememberLoggingAdapterSpec extends UtilsSpec with LoggingAdapterFixture {

  behavior of "RememberLoggingAdapter"

  it should "remember infos" in {
    val remember = useRememberAdapter(recordMessage)
    assertLogsAsExpected(remember.records)
  }
}

class RememberOrNormalAdapterSpec extends UtilsSpec with LoggingAdapterFixture with LocalOpsPimper[Future] {
  behavior of "RememberOrNormalAdapter"

  it should " use the normal adapter" in {
    val remember = useRememberOrNormalAdapter(recordMessage)
    assertLogsAsExpected(remember.records)
  }

  it should "use the normal adapter as well as the one on the local" in {
    val remember = useRememberOrNormalAdapter { mainAdapter =>
      val localStoreAdapter = useRememberAdapter { rememberAdapter =>
        putInlocalStore[LoggingAdapter](rememberAdapter)
        recordMessage(mainAdapter)
      }
      assertLogsAsExpected(localStoreAdapter.records)
    }
    assertLogsAsExpected(remember.records)
  }
}