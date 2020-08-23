/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.logging


import one.xingyi.core.local.LocalOpsPimper
import one.xingyi.core.time.MockTimeService
import one.xingyi.core.{CoreSpec, UtilsSpec}

import scala.concurrent.Future
import scala.language.higherKinds

trait LoggingAdapterFixture extends CoreSpec {
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
