package org.validoc.utils.caching

import org.validoc.utils.logging.Logging
import org.validoc.utils.map.{MapSizeStrategy, MaxMapSizeStrategy, NoMapSizeStrategy}
import org.validoc.utils.time.{NanoTimeService, SystemClockNanoTimeService}

import language.higherKinds
import scala.concurrent.duration.Duration
import scala.util.Try


trait CachingStrategy[M[_], Req, Id, Res] {
  def bypassCache: Req => Boolean

  def id: Req => Id

  def staleCachingStrategy: StaleCacheStrategy

  def shouldCacheStrategy: Try[Res] => Boolean

  def nanoTimeService: NanoTimeService

}

object CachingStrategy {
  def apply[M[_], Req, Id, Res](id: Req => Id, timeToStale: Duration,
                                timeToDead: Duration,
                                bypassCache: Req => Boolean,
                                nameTimeService: NanoTimeService = SystemClockNanoTimeService,
                                shouldCacheStrategy: (Try[Res] => Boolean) = (_: Try[Res]) => true) =
    new SimpleCachingStrategy[M, Req, Id, Res](
      id = id,
      staleCachingStrategy = DurationStaleCacheStategy(timeToStale.toNanos, timeToDead.toNanos),
      bypassCache = bypassCache,
      shouldCacheStrategy = shouldCacheStrategy,
      nanoTimeService = nameTimeService
    )
}

class SimpleCachingStrategy[M[_], Req, Id, Res](val id: Req => Id,
                                                val staleCachingStrategy: StaleCacheStrategy,
                                                val bypassCache: Req => Boolean,
                                                val shouldCacheStrategy: Try[Res] => Boolean,
                                                val nanoTimeService: NanoTimeService) extends CachingStrategy[M, Req, Id, Res]


trait StaleCacheStrategy {
  def state[M[_]](cacheName: String)(timeService: NanoTimeService)(cachedValue: CachedValue[M, _]): StaleState
}

sealed trait StaleState

object Fresh extends StaleState

object Stale extends StaleState

object Dead extends StaleState

case class DurationStaleCacheStategy(staleTimeInNanos: Long, deadTimeInNanos: Long) extends StaleCacheStrategy with Logging {

  override def state[M[_]](cacheName: String)(timeService: NanoTimeService)(cachedValue: CachedValue[M, _]): StaleState = {
    val now = timeService()
    val staleTime = cachedValue.time + staleTimeInNanos
    val deadTime = cachedValue.time + deadTimeInNanos
    val result = if (now < staleTime) Fresh else if (now < deadTime) Stale else Dead

    trace(f"DurationStaleCacheStategy($this) now: ${now / 1000000000.0}%5.2f staleTime ${staleTimeInNanos / 1000000000.0}%5.2f, seconds used up ${(now - cachedValue.time) / 1000000000.0}%5.2f result: $result")
    result
  }

}