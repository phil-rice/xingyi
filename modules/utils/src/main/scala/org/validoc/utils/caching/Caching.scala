package org.validoc.utils.caching

import java.util.concurrent.atomic.{AtomicLong, AtomicReference}
import java.util.function.UnaryOperator

import org.validoc.utils.Service
import org.validoc.utils.concurrency.{DoubleCheckLock, Futurable}
import org.validoc.utils.logging.Logging

import scala.language.higherKinds
import scala.util.Try


trait ModifyCache[K, V] {
  def apply(k: K)(fn: V => V): V
}

class LowLevelSafeMap[K, V](default: => V) extends ModifyCache[K, V] {
  def clear = initialValueLock(true)(_map = Map[K, AtomicReference[V]]())

  private val initialValueLock = new DoubleCheckLock

  private var _map = Map[K, AtomicReference[V]]()

  def copyOfMap = _map

  private def get(k: K): AtomicReference[V] = {
    initialValueLock(!_map.contains(k))(_map = _map + (k -> new AtomicReference(default)))
    _map(k)
  }

  def apply(k: K)(fn: V => V): V = {
    get(k).updateAndGet(new UnaryOperator[V] {
      override def apply(t: V) = fn(t)
    })
  }
}

trait CachingOps {
  def name: String

  def clearCache

  def cachingMetrics: CachingMetricSnapShot
}

class CachingService[M[_] : Futurable, Req, Id, Res](val name: String, delegate: Service[M, Req, Res], cachingStrategy: CachingStrategy[M, Req, Id, Res]) extends Service[M, Req, Res] with CachingOps with Logging {

  import cachingStrategy._

  private implicit val futurable = implicitly[Futurable[M]]
  private val nextId = new AtomicLong()
  private val map = new LowLevelSafeMap[Id, CachedValue[M, Res]](CachedValue[M, Res](nanoTimeService(), CachedId(nextId.getAndIncrement()), None, None))
  private val metrics = CachingMetrics()

  override def clearCache = map.clear

  private def logRequest(request: Req, whatsHappening: String) = debug(s" Cache $name. $whatsHappening for request $request")

  private def staleState = cachingStrategy.staleCachingStrategy.state[M](name)(nanoTimeService) _

  private def recordResult(req: Req, c: CachedValue[M, Res])(tryRes: Try[Res]) = {
    map(id(req)) { cNew =>
      tryRes.fold(_ => metrics.delegateFailures, _ => metrics.delegateSuccesses).incrementAndGet()
      if (cNew.inFlightId != c.inFlightId) cNew
      else if (shouldCacheStrategy(tryRes)) cNew.copy(time = nanoTimeService(), inFlight = None, value = Some(futurable.liftTry(tryRes)))
      else cNew.copy(inFlight = None)
    }
  }

  private def askDelegate(req: Req, c: CachedValue[M, Res]) = {
    metrics.delegateRequests.incrementAndGet()
    val delegateResult: M[Res] = delegate(req)
    val finalResult = futurable.registerSideEffectWhenComplete(delegateResult, recordResult(req, c))
    c.copy(inFlight = Some(finalResult))
  }


  private def sendToDelegateIfNeededUpdatingCache(req: Req)(c: CachedValue[M, Res]): CachedValue[M, Res] = {

    (c, staleState(c)) match {
      case (c@CachedValue(_, _, None, _), Dead) =>
        metrics.deadRequest.incrementAndGet()
        logRequest(req, s"Have dead data, and nothing intransit. Time to nuke the dead data,  and request new")
        askDelegate(req, c.copy(value = None))
      case (c@CachedValue(_, _, Some(intransit), None), Dead) =>
        logRequest(req, s"Have dead data, but something intransit, so using intransit")
        c
      case (c@CachedValue(__, _, None, None), _) =>
        logRequest(req, s"No data in the cache. Launching a upstream request")
        askDelegate(req, c)
      case (c@CachedValue(__, _, Some(inTransit), None), _) =>
        logRequest(req, s"There are no successful responses yet, but there is an intransit")
        c
      case (c@CachedValue(_, _, _, Some(future)), Fresh) =>
        logRequest(req, s"Have fresh data, so using it")
        c
      case (c@CachedValue(_, _, None, Some(future)), Stale) =>
        logRequest(req, s"Have stale data, this is first time. Requesting new")
        askDelegate(req, c)
      case (c@CachedValue(_, _, Some(inTransit), _), Stale) =>
        logRequest(req, s"Have stale data, already have intransit, so using stale data")
        c
      case (_, s) => throw new RuntimeException(s"unexpected state. stale $s value $c")
    }
  }

  private def cachedRequest(req: Req): M[Res] =
    map(id(req))(sendToDelegateIfNeededUpdatingCache(req)).valueToUse


  override def apply(req: Req): M[Res] =
    if (bypassCache(req)) {
      metrics.bypassedRequests.incrementAndGet()
      delegate(req)
    } else {
      metrics.requests.incrementAndGet()
      cachedRequest(req)
    }

  override def cachingMetrics: CachingMetricSnapShot = metrics.snapshot(name, map.copyOfMap, cachingStrategy.sizeStrategy.toString)
}
