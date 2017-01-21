package org.validoc.utils.caching

import org.validoc.utils.logging.Logging
import org.validoc.utils.time.NanoTimeService

sealed trait StaleState

object Fresh extends StaleState

object Stale extends StaleState

object Dead extends StaleState

trait StaleCacheStrategy {
  def state[M[_]](cacheName: String)(timeService: NanoTimeService)(cachedValue: CachedValue[M, _]): StaleState
}

case class DurationStaleCacheStategy(staleTimeInNanos: Long, deadTimeInNanos: Long) extends StaleCacheStrategy with Logging {

  override def state[M[_]](cacheName: String)(timeService: NanoTimeService)(cachedValue: CachedValue[M, _]) = {
    val now = timeService()
    val staleTime = cachedValue.time + staleTimeInNanos
    val deadTime = cachedValue.time + deadTimeInNanos
    val result = if (now < staleTime) Fresh else if (now < deadTime) Stale else Dead

    trace(f"DurationStaleCacheStategy($cacheName) staleTime ${staleTimeInNanos / 1000000000}%5.2f, seconds used up ${(now - cachedValue.time) / 1000000000}%5.2f result: $result")
    result
  }

}
