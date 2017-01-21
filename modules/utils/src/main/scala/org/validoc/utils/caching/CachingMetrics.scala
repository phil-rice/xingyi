package org.validoc.utils.caching

import java.util.concurrent.atomic.AtomicLong

import org.validoc.utils.map.ReportMapSizeChange


case class CachingMetrics(bypassedRequests: AtomicLong = new AtomicLong,
                          requests: AtomicLong = new AtomicLong,
                          staleRequest: AtomicLong = new AtomicLong,
                          deadRequest: AtomicLong = new AtomicLong,
                          inTransitRequests: AtomicLong = new AtomicLong,
                          inTransitSucesses: AtomicLong = new AtomicLong,
                          inTransitFailures: AtomicLong = new AtomicLong,
                          removedBecauseTooFull: AtomicLong = new AtomicLong) extends ReportMapSizeChange {
  def snapshot[K, V](name: String, cache: Map[K, V], sizeString: String) = {
    val (inTransit, cachedResults, inTransitWithoutResults) = cache.foldLeft((0, 0, 0)) {
      case ((inTransit, cachedResults, inTransitWithoutResults), (req, CachedValue(_, _, inTransitOption, valueOption))) =>
        (inTransit + inTransitOption.fold(0)(_ => 1),
          cachedResults + valueOption.fold(0)(_ => 1),
          inTransitWithoutResults + valueOption.fold(inTransitOption.fold(0)(_ => 1))(_ => 0))
    }
    CachingMetricSnapShot(name, bypassedRequests.get(), requests.get, staleRequest.get, deadRequest.get(),
      inTransitRequests.get, inTransitSucesses.get, inTransitFailures.get,
      cache.size, inTransit, cachedResults, inTransitWithoutResults, removedBecauseTooFull.get(), sizeString
    )

  }

  override def mapSizeChanges[K](oldSize: Int, keysRemoved: Iterable[K]): Unit = removedBecauseTooFull.addAndGet(keysRemoved.size)
}


case class CachingMetricSnapShot(name: String,
                                 bypassedRequests: Long,
                                 requests: Long,
                                 staleRequest: Long,
                                 deadRequest: Long,
                                 inTransitRequests: Long,
                                 inTransitSucesses: Long,
                                 inTransitFailures: Long,
                                 cacheSize: Int,
                                 inTransits: Int,
                                 cachedResults: Int,
                                 inTransitWithoutResults: Int,
                                 removedBecauseTooFull: Long, cacheSizeStrategyString: String) {
  override def toString: String =
    s"CachingMetricSnapShot($name,bypassedRequests=$bypassedRequests,requests=$requests,staleRequest=$staleRequest,deadRequest=$deadRequest,inTransitRequests=$inTransitRequests," +
      s"inTransitSucesses=$inTransitSucesses,inTransitFailures=$inTransitFailures,bypassedRequests=$bypassedRequests,cacheSize=$cacheSize," +
      s"inTransits=$inTransits,cachedResults=$cachedResults,inTransitWithoutResults=$inTransitWithoutResults," +
      s"removedBecauseTooFull=$removedBecauseTooFull" +
      s"cacheSizeStrategy=[${cacheSizeStrategyString}]"

}