/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.profiling

import java.util.concurrent.atomic.{AtomicLong, AtomicReference}

import one.xingyi.core.UtilsWithLoggingSpec
import one.xingyi.core.time.{MockTimeService, NanoTimeService}

import scala.concurrent.duration._
import scala.language.{implicitConversions, postfixOps}
import scala.util.{Failure, Success, Try}

class TryProfileDataTest extends UtilsWithLoggingSpec {
  behavior of "TryProfileData"

  implicit def durationToNanos(d: Duration) = d.toNanos

  val success = Success(1)
  val failure = Failure(new RuntimeException)

  it should "separately record success and failure times" in {
    val pd = new TryProfileData

    pd.event(100 micros)(success)
    pd.event(100 micros)(success)
    pd.event(10 millis)(failure)
    pd.event(10 millis)(failure)
    pd.event(10 millis)(success)
    pd.event(200 millis)(failure)
    pd.event(200 millis)(success)
    pd.event(2 second)(failure)
    pd.event(3 second)(success)
    pd.succeededData.shortToString shouldBe "  642.04ms/5        0.10ms/2       10.00ms/1      200.00ms/1    3,000.00ms/1   "
    pd.failedData.shortToString shouldBe "  555.00ms/4        0.00ms/0       10.00ms/2      200.00ms/1    2,000.00ms/1   "
    pd.toShortString should startWith(pd.succeededData.shortToString)
    pd.toShortString should endWith(pd.failedData.shortToString)
  }
  it should "it should delegate to event based on the start time with eventFromStartTime" in {
    val atomicLong = new AtomicLong
    val atomicRef = new AtomicReference[Try[_]]()
    implicit val nanoTimeService = new MockTimeService
    val pd = new TryProfileData {
      override def event[Res](nanos: Long)(result: Try[Res])(implicit profileAs: ProfileAs[Res]) = {
        atomicLong.set(nanos)
        atomicRef.set(result)
      }
    }
    pd.eventFromStartTime(400)(Try("result"))
    atomicLong.get shouldBe 700
    atomicRef.get shouldBe Try("result")
  }

  it should "clearData and allow more recordings" in {
    val pd = new TryProfileData

    pd.event(100 micros)(success)
    pd.event(100 micros)(success)
    pd.event(10 millis)(failure)
    pd.event(10 millis)(failure)
    pd.event(10 millis)(success)
    pd.event(200 millis)(failure)
    pd.event(200 millis)(success)
    pd.clearData
    pd.event(2 second)(failure)
    pd.event(3 second)(success)
    pd.succeededData.shortToString shouldBe "3,000.00ms/1        0.00ms/0        0.00ms/0        0.00ms/0    3,000.00ms/1   "
    pd.failedData.shortToString shouldBe "2,000.00ms/1        0.00ms/0        0.00ms/0        0.00ms/0    2,000.00ms/1   "
  }
}
