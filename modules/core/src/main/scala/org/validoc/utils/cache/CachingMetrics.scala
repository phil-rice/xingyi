package org.validoc.utils.cache


import java.util.concurrent.atomic.AtomicLong

import org.validoc.utils.map.{ReportMapSizeReduction, SafeMap}


case class CachingMetrics(bypassedRequests: AtomicLong = new AtomicLong,
                          requests: AtomicLong = new AtomicLong,
                          staleRequest: AtomicLong = new AtomicLong,
                          deadRequest: AtomicLong = new AtomicLong,
                          delegateRequests: AtomicLong = new AtomicLong,
                          delegateSuccesses: AtomicLong = new AtomicLong,
                          delegateFailures: AtomicLong = new AtomicLong,
                          removedBecauseTooFull: AtomicLong = new AtomicLong) extends ReportMapSizeReduction {
  def snapshot[K, V](name: String, cache: SafeMap[K, V], sizeString: String) = {
    val (inTransit, cachedResults, inTransitWithoutResults) = cache.foldLeft((0, 0, 0)) {
      case ((inTransit, cachedResults, inTransitWithoutResults), (req, CachedValue(_, _, inTransitOption, valueOption))) =>
        (inTransit + inTransitOption.fold(0)(_ => 1),
          cachedResults + valueOption.fold(0)(_ => 1),
          inTransitWithoutResults + valueOption.fold(inTransitOption.fold(0)(_ => 1))(_ => 0))
    }
    CachingMetricSnapShot(name, bypassedRequests.get(), requests.get, staleRequest.get, deadRequest.get(),
      delegateRequests.get, delegateSuccesses.get, delegateFailures.get,
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
                                 delegateRequests: Long,
                                 delegateSuccesses: Long,
                                 delegateFailures: Long,
                                 cacheSize: Int,
                                 inTransits: Int,
                                 cachedResults: Int,
                                 inTransitWithoutResults: Int,
                                 removedBecauseTooFull: Long, cacheSizeStrategyString: String) {
  override def toString: String =
    s"CachingMetricSnapShot($name,bypassedRequests=$bypassedRequests,requests=$requests,staleRequest=$staleRequest,deadRequest=$deadRequest,delegateRequests=$delegateRequests," +
      s"delegateSuccesses=$delegateSuccesses,delegateFailures=$delegateFailures,bypassedRequests=$bypassedRequests,cacheSize=$cacheSize," +
      s"inTransits=$inTransits,cachedResults=$cachedResults,inTransitWithoutResults=$inTransitWithoutResults," +
      s"removedBecauseTooFull=$removedBecauseTooFull" +
      s"cacheSizeStrategy=[${cacheSizeStrategyString}]"

}