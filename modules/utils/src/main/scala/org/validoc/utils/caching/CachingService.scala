package org.validoc.utils.caching

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicLong

import org.validoc.utils.Futurable
import org.validoc.utils.logging.Logging
import org.validoc.utils.map.{MapSizeStrategy, MaxMapSizeStrategy}
import org.validoc.utils.service.WrappingService
import org.validoc.utils.time.NanoTimeService

case class CachedId(id: Long)

case class CachedValue[M[_], T](time: Long, cachedId: CachedId, inTransit: Option[M[T]], value: Option[M[T]])


trait CachingProperties[T] {
  def bypassCache(t: T): Boolean
}

case class CachingConfig[M[_]](timeService: NanoTimeService, staleCacheStrategy: StaleCacheStrategy)(implicit val futurable: Futurable[M]) {
  def withNameAndSize(name: String, size: Int) = CachingConfigWithNameAndSize(this, name, new MaxMapSizeStrategy(size, Math.max(size / 10, 1)))
}

case class CachingConfigWithNameAndSize[M[_]](cachingConfig: CachingConfig[M], name: String, sizeStrategy: MapSizeStrategy)(implicit val futurable: Futurable[M])


object CachingService {
  private val lock = new Object

  private var caches: List[CachingOps] = Nil

  def addCachingService(cachingService: CachingOps): Unit = {
    lock.synchronized {
      caches = cachingService :: caches
    }
  }

  def clearCaches(): Unit = caches.foreach(_.clearCache)

  def getMetrics(): String = caches.foldLeft("")((acc, c) => s"${acc}\n${c.name}:\n${c.cachingMetrics}\n")
}

trait CachingOps {
  def name: String

  def clearCache

  def cachingMetrics: CachingMetricSnapShot
}

class CachingService[M[_] : Futurable, Req, Res](delegate: Req => M[Res], cachingConfigWithName: CachingConfigWithNameAndSize[M], addToGlobal: Boolean = true)(implicit cachingProperties: CachingProperties[Req]) extends WrappingService[M, Req, Res](cachingConfigWithName.name, delegate) with CachingOps with Logging {

  import Futurable._

  private val cachingMetricsData = CachingMetrics()
  private val futurable = implicitly[Futurable[M]]

  override val name = cachingConfigWithName.name

  if (addToGlobal) CachingService.addCachingService(this)

  private var cache = Map[Req, CachedValue[M, Res]]()

  private val lock = new Object
  private val nextId = new AtomicLong()

  private val timeService = cachingConfigWithName.cachingConfig.timeService

  private def staleState = cachingConfigWithName.cachingConfig.staleCacheStrategy.state[M](name)(timeService) _


  def clearCache: Unit = lock.synchronized(cache = Map[Req, CachedValue[M, Res]]())

  protected def copyOfCache = lock.synchronized(cache)

  def cachingMetrics: CachingMetricSnapShot =
    cachingMetricsData.snapshot(name, cache, cachingConfigWithName.sizeStrategy.toString)

  protected def addToCache(thisId: CachedId, request: Req, response: Res) =
    lock.synchronized {
      def addToCache = cache += (request -> CachedValue(timeService(), thisId, None, Some(futurable.lift(response))))

      cachingMetricsData.inTransitSucesses.incrementAndGet()

      cache.get(request) match {
        case Some(CachedValue(time, id, _, _)) if thisId == id =>
          debug(s"A 'inTransit' for $name has suceeded with $thisId. Request was $request\nIt changed the cache so that there is nothing in transit and new value is $response")
          addToCache
        case Some(CachedValue(time, id, _, _)) =>
          debug(s"A 'inTransit' for $name has suceeded with $thisId. Request was $request\nBUT!!! The id in the expected cachevalue was $id. Was the cache cleared or was this a race condition being prevented?")
        case None =>
          debug(s"A 'inTransit' for $name has suceeded with $thisId. Request was $request\nBUT!!! There was no cached value to put it into. Was the cache cleared?")

      }
    }

  protected def failed(thisId: CachedId, request: Req, a: Any) = {
    def logError(message: String): Unit = a match {
      case t: Throwable => error(message, t)
      case other => error(message + ", " + other)
    }

    lock.synchronized {
      cachingMetricsData.inTransitFailures.incrementAndGet()
      cache.get(request) match {
        case Some(CachedValue(time, id, inTransit, future)) if id == thisId =>
          cache += (request -> CachedValue(time, id, None, future))
          logError(s"A 'inTransit' for $name  has failed with $thisId. It has removing itself from the cache. Request was $request")
        case _ =>
          logError(s"A 'inTransit' for $name  has failed with $thisId. As a new intransit has already been sent, it is not doing anything to the cache")
      }
    }
  }

  protected def update(request: Req, c: Option[CachedValue[M, Res]]): M[Res] =
    lock.synchronized {
      val thisId = CachedId(nextId.getAndIncrement())
      val latch = new CountDownLatch(1)
      val intransit = createInTransit(request, thisId, latch)
      cache.get(request).fold {
        cache += (request -> CachedValue(0, thisId, Some(intransit), None))
      } {
        case CachedValue(time, _, _, future) => cache += (request -> CachedValue(time, thisId, Some(intransit), future))
      }
      latch.countDown()
      intransit
    }

  protected def createInTransit(request: Req, thisId: CachedId, latch: CountDownLatch): M[Res] = {
    futurable.launch {
      latch.await()
      cachingMetricsData.inTransitRequests.incrementAndGet()
      debug(s"Launching an intransit for $name  with $thisId request is $request")
    }.flatMap { _ => delegate(request) }
      .onComplete(addToCache(thisId, request, _), failed(thisId, request, _))
  }

  private def logRequest(request: Req, whatsHappening: String) = debug(s" $whatsHappening for request $request")

  override def apply(request: Req): M[Res] = {
    if (cachingProperties.bypassCache(request)) {
      logRequest(request, s"Bypassing Cache for $name ")
      cachingMetricsData.bypassedRequests.incrementAndGet()
      delegate(request)
    }
    else lock.synchronized {
      cachingMetricsData.requests.incrementAndGet()
      cache = cachingConfigWithName.sizeStrategy.modifyCache(cache, cachingMetricsData)
      cache.get(request) match {
        case None =>
          logRequest(request, s"Cache $name. As it's first request: updating")
          update(request, None) //only at the start
        case Some(c@CachedValue(__, _, None, None)) =>
          logRequest(request, s"Cache $name. There has been previous attempts, but none has worked yet: updating")
          update(request, Some(c))
        case Some(c@CachedValue(__, _, Some(inTransit), None)) =>
          logRequest(request, s"Cache $name. There are no successful responses yet, so returning inTransit")
          inTransit
        case Some(c@CachedValue(_, _, _, Some(future))) if (staleState(c)) == Fresh =>
          logRequest(request, s"Cache $name. Using cached value,")
          future
        case Some(c@CachedValue(_, _, None, Some(future))) if (staleState(c)) == Stale =>
          cachingMetricsData.staleRequest.incrementAndGet()
          logRequest(request, s"Cache $name. Using cached value. It is stale, and this is the first time a stale response has been request, so a new intransit query has been made")
          update(request, Some(c))
          future
        case Some(c@CachedValue(_, _, Some(inTransit), Some(future))) if (staleState(c)) == Stale =>
          cachingMetricsData.staleRequest.incrementAndGet()
          logRequest(request, s"Cache $name. Using cached value. It is stale, but there is already a request for a new one in transit")
          future
        case Some(c@CachedValue(_, _, _, _)) if (staleState(c)) == Dead =>
          cachingMetricsData.deadRequest.incrementAndGet()
          logRequest(request, s"Cache $name. The cached value is not just stale, it's dead. Removing from cache and replacing")
          cache = cache - request
          update(request, None)
      }
    }
  }
}