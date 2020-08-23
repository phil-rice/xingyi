/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.cache

import one.xingyi.core.time.NanoTimeService

import scala.language.higherKinds


trait StaleCacheStrategy {
  def state[M[_]](cacheName: String)(cachedValue: CachedValue[M, _])(implicit timeService: NanoTimeService): StaleState
}

sealed trait StaleState

object Fresh extends StaleState

object Stale extends StaleState

object Dead extends StaleState

case class DurationStaleCacheStategy(staleTimeInNanos: Long, deadTimeInNanos: Long) extends StaleCacheStrategy {

  override def state[M[_]](cacheName: String)(cachedValue: CachedValue[M, _])(implicit timeService: NanoTimeService): StaleState = {
    val now = timeService()
    val staleTime = cachedValue.time + staleTimeInNanos
    val deadTime = cachedValue.time + deadTimeInNanos
    val result = if (now < staleTime) Fresh else if (now < deadTime) Stale else Dead

    //    trace(f"DurationStaleCacheStategy($this) now: ${now / 1000000000.0}%5.2f staleTime ${staleTimeInNanos / 1000000000.0}%5.2f, seconds used up ${(now - cachedValue.time) / 1000000000.0}%5.2f result: $result")
    result
  }

}
