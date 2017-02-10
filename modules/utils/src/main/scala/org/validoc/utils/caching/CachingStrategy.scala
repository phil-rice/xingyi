package org.validoc.utils.caching

import org.validoc.utils.logging.Logging
import org.validoc.utils.time.{NanoTimeService, SystemClockNanoTimeService}

import scala.concurrent.duration.Duration
import scala.language.higherKinds
import scala.util.Try



trait StaleCacheStrategy {
  def state[M[_]](cacheName: String)(cachedValue: CachedValue[M, _])(implicit timeService: NanoTimeService): StaleState
}

sealed trait StaleState

object Fresh extends StaleState

object Stale extends StaleState

object Dead extends StaleState

case class DurationStaleCacheStategy(staleTimeInNanos: Long, deadTimeInNanos: Long) extends StaleCacheStrategy with Logging {

  override def state[M[_]](cacheName: String)(cachedValue: CachedValue[M, _])(implicit timeService: NanoTimeService): StaleState = {
    val now = timeService()
    val staleTime = cachedValue.time + staleTimeInNanos
    val deadTime = cachedValue.time + deadTimeInNanos
    val result = if (now < staleTime) Fresh else if (now < deadTime) Stale else Dead

    trace(f"DurationStaleCacheStategy($this) now: ${now / 1000000000.0}%5.2f staleTime ${staleTimeInNanos / 1000000000.0}%5.2f, seconds used up ${(now - cachedValue.time) / 1000000000.0}%5.2f result: $result")
    result
  }

}