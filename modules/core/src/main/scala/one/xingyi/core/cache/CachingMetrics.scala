/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.cache


import java.util.concurrent.atomic.AtomicLong

import one.xingyi.core.map.{ReportMapSizeReduction, SafeMap}


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
