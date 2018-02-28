package org.validoc.utils.cache

import org.validoc.utils.time.NanoTimeService

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