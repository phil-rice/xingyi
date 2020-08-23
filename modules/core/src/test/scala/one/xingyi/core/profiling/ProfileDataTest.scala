/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.profiling

import one.xingyi.core.UtilsWithLoggingSpec

import scala.concurrent.duration.{Duration, _}
import scala.language.{implicitConversions, postfixOps}

class ProfileDataTest extends UtilsWithLoggingSpec {
  behavior of "ProfileData"

  implicit def durationToNanos(d: Duration) = d.toNanos

  it should "put the durations into different buckets" in {
    val pd = new ProfileData
    pd.event(100 micros)
    pd.event(100 micros)
    pd.event(10 millis)
    pd.event(10 millis)
    pd.event(10 millis)
    pd.event(200 millis)
    pd.event(200 millis)
    pd.event(2 second)
    pd.event(2 second)
    pd.shortToString shouldBe "  492.24ms/9        0.10ms/2       10.00ms/3      200.00ms/2    2,000.00ms/2   "
  }

  it should "clear data and then accumulate data again" in {
    val pd = new ProfileData
    pd.event(10 millis)
    pd.event(200 millis)
    pd.clearData
    pd.event(1 second)
    pd.shortToString shouldBe "1,000.00ms/1        0.00ms/0        0.00ms/0        0.00ms/0    1,000.00ms/1   "

  }
}


