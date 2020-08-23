/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.time

import one.xingyi.core.UtilsWithLoggingSpec

import scala.concurrent.duration._
import scala.language.postfixOps
class TimeSpec extends UtilsWithLoggingSpec {
  behavior of "Delay"

  it should "have a delay within 50% of the specified duration" in {
    val rd = new RandomDelay(5 seconds)
    val duration1 = rd()
    val duration2 = rd()
    duration1 shouldNot equal(duration2)
    val seconds8 = (8 seconds).toNanos
    val seconds2 = (2 seconds).toNanos

    1 to 100 forall { _ =>
      val d = rd().toNanos
      d < seconds8 && d > seconds2
    }
  }

  behavior of "NanoTimeService"

  it should "return System.nanos" in {
    val x = implicitly[NanoTimeService].apply()
    val expected = System.nanoTime()

    Math.abs(x-expected) < 1000000000l shouldBe true
  }
}
