package org.validoc.utils.caching2

import java.util.concurrent.atomic.{AtomicLong, AtomicReference}
import java.util.function.UnaryOperator

import org.validoc.utils.Service
import org.validoc.utils.caching._
import org.validoc.utils.concurrency.{DoubleCheckLock, Futurable}
import org.validoc.utils.logging.Logging
import org.validoc.utils.time.NanoTimeService

import scala.util.Try
import language.higherKinds

trait CachingStrategy[M[_], Req, Id, Res] {
  def bypassCache(req: Req): Boolean

  def id(req: Req): Id

  def shouldCacheStrategy: ShouldCacheStrategy[Res]

  def nanoTimeService: NanoTimeService

  def staleCachingStrategy: StaleCacheStrategy2

}

trait ShouldCacheStrategy[Res] {
  def shouldCacheResult(res: Try[Res]): Boolean
}


trait StaleCacheStrategy2 {
  def state[M[_]](cacheName: String)(timeService: NanoTimeService)(cachedValue: CachedValue2[M, _]): StaleState
}

case class CachedValue2[M[_], T](time: Long, inFlightId: CachedId, inFlight: Option[M[T]], value: Option[M[T]]) {
  def valueToUse: M[T] = value.getOrElse(inFlight.getOrElse(throw new RuntimeException("Should not get this. trying to return future from CachedValue")))

}

trait ModifyCache[K, V] {
  def apply(k: K)(fn: V => V): V
}

class LowLevelSafeMap[K, V](default: => V) extends ModifyCache[K, V] {
  private val initialValueLock = new DoubleCheckLock

  private var map = Map[K, AtomicReference[V]]()

  private def get(k: K): AtomicReference[V] = {
    initialValueLock(!map.contains(k))(map = map + (k -> new AtomicReference(default)))
    map(k)
  }

  def apply(k: K)(fn: V => V): V = {
    get(k).updateAndGet(new UnaryOperator[V] {
      override def apply(t: V) = fn(t)
    })
  }

}

class Cache[M[_] : Futurable, Req, Id, Res](name: String, delegate: Service[M, Req, Res], cachingStrategy: CachingStrategy[M, Req, Id, Res]) extends Service[M, Req, Res] with Logging {

  import cachingStrategy._

  private implicit val futurable = implicitly[Futurable[M]]

  private val nextId = new AtomicLong()

  private val map = new LowLevelSafeMap[Id, CachedValue2[M, Res]](CachedValue2[M, Res](nanoTimeService(), CachedId(nextId.getAndIncrement()), None, None))

  private def logRequest(request: Req, whatsHappening: String) = debug(s" Cache $name. $whatsHappening for request $request")

  private def staleState = cachingStrategy.staleCachingStrategy.state[M](name)(nanoTimeService) _

  private def recordResult(req: Req, c: CachedValue2[M, Res])(tryRes: Try[Res]) = {
    map(id(req)) { cNew =>
      if (cNew.inFlightId != c.inFlightId) cNew
      else if (shouldCacheStrategy.shouldCacheResult(tryRes)) cNew.copy(inFlight = None, value = Some(futurable.liftTry(tryRes)))
      else cNew.copy(inFlight = None)
    }
  }

  private def askDelegate(req: Req, c: CachedValue2[M, Res]) = {
    val delegateResult: M[Res] = delegate(req)
    val finalResult = futurable.registerSideEffectWhenComplete(delegateResult, recordResult(req, c))
    c.copy(inFlight = Some(finalResult))
  }


  private def sendToDelegateIfNeededUpdatingCache(req: Req)(c: CachedValue2[M, Res]): CachedValue2[M, Res] = {
    (c, staleState(c)) match {
      case (c@CachedValue2(_, _, None, _), Dead) =>
        logRequest(req, s"Have dead data, and nothing intransit. Time to nuke the dead data,  and request new")
        askDelegate(req, c.copy(value = None))
      case (c@CachedValue2(_, _, Some(intransit), None), Dead) =>
        logRequest(req, s"Have dead data, but something intransit, so using intransit")
        c
      case (c@CachedValue2(__, _, None, None), _) =>
        logRequest(req, s"No data in the cache. Launching a upstream request")
        askDelegate(req, c)
      case (c@CachedValue2(__, _, Some(inTransit), None), _) =>
        logRequest(req, s"There are no successful responses yet, but there is an intransit")
        c
      case (c@CachedValue2(_, _, _, Some(future)), Fresh) =>
        logRequest(req, s"Have fresh data, so using it")
        c
      case (c@CachedValue2(_, _, None, Some(future)), Stale) =>
        logRequest(req, s"Have stale data, this is first time. Requesting new")
        askDelegate(req, c)
      case (c@CachedValue2(_, _, Some(inTransit), _), Stale) =>
        logRequest(req, s"Have stale data, already have intransit, so using stale data")
        c
      case (c, s) => throw new RuntimeException(s"unexpected state. stale $s value $c")
    }
  }

  private def cachedRequest(req: Req): M[Res] = {
    map(id(req))(sendToDelegateIfNeededUpdatingCache(req)).valueToUse
  }


  override def apply(req: Req): M[Res] = {
    if (bypassCache(req)) delegate(req) else cachedRequest(req)
  }
}
