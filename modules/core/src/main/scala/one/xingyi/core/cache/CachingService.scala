package one.xingyi.core.cache

import java.util.concurrent.atomic.AtomicLong

import one.xingyi.core.functions.MonadWithException
import one.xingyi.core.language.Language._
import one.xingyi.core.map.{MapSizeStrategy, SafeMap}
import one.xingyi.core.time.NanoTimeService

import scala.language.higherKinds
import scala.util.Try


trait CachableKey[Req] {
  def id(req: Req): Id

  def bypassCache(req: Req): Boolean
}

object CachableKey {
  implicit def cachableKeyDefault[T] = new CachableKey[T] {
    override def id(req: T): Id = ObjectId(req)

    override def bypassCache(req: T): Boolean = false
  }

  implicit object CachableKeyForUnit extends CachableKey[Unit] {
    override def id(req: Unit): Id = UnitId

    override def bypassCache(req: Unit): Boolean = false
  }

}


trait CachingInfoAndOps {
  def name: String
  def clear
  def cachingMetrics: CachingMetricSnapShot
}

class CachingServiceFactory[M[_] : MonadWithException](cachingStrategy: StaleCacheStrategy,
                                                       sizeStrategy: MapSizeStrategy)(implicit timeService: NanoTimeService) extends CacheFactory[M] {
  override def apply[Req: CachableKey, Res: ShouldCacheResult](name: String, raw: Req => M[Res]): Cache[M, Req, Res] = {
    new CachingService[M, Req, Res](name, raw, cachingStrategy, sizeStrategy)
  }
}


class CachingService[M[_] : MonadWithException, Req: CachableKey, Res: ShouldCacheResult](val name: String,
                                                                                          val raw: (Req => M[Res]),
                                                                                          protected val cachingStrategy: StaleCacheStrategy,
                                                                                          sizeStrategy: MapSizeStrategy)
                                                                                         (implicit timeService: NanoTimeService)
  extends (Req => M[Res]) with Cache[M, Req, Res] with HasCachingCommands[M, Req, Res] with CachingInfoAndOps {

  val cachableKey = implicitly[CachableKey[Req]]
  val cachableResult = implicitly[ShouldCacheResult[Res]]

  private val nextId = new AtomicLong()
  protected val metrics = CachingMetrics()
  protected val map =
    SafeMap[Id, CachedValue[M, Res]](default = CachedValue[M, Res](timeService(), CachedId(nextId.getAndIncrement()), None, None), sizeStrategy, metrics)

  override def clear = map.clear

  //  private def logRequest(request: Req, whatsHappening: String) = debug(s" Cache $name. $whatsHappening for request $request")

  private def staleState = cachingStrategy.state[M](name) _

  override def cachingMetrics: CachingMetricSnapShot = metrics.snapshot(name, map, sizeStrategy.toString)

  private def recordResult(req: Req, c: CachedValue[M, Res])(tryRes: Try[Res]): CachedValue[M, Res] = {
    //    trace(s"recordResult($req, $c)($tryRes)")
    map(cachableKey.id(req)) { cNew =>
      try {
        if (cNew.inFlightId != c.inFlightId) cNew
        else if (cachableResult.shouldCacheStrategy(tryRes)) cNew.copy(time = timeService(), inFlight = None, value = Some(liftTry(tryRes)))
        else cNew.copy(inFlight = None)
      } finally {
        (if (tryRes.isFailure) metrics.delegateFailures else metrics.delegateSuccesses).incrementAndGet()
      }
    }
  }

  private def sendDelegateFor(command: NeedsToSendDelegateCacheCommand) = {
    val cachedValue = command.cachedValue
    val req = command.req
    metrics.delegateRequests.incrementAndGet()
    val delegateResult: M[Res] = raw(req)
    val finalResult = delegateResult.registerSideeffect(recordResult(req, cachedValue))
    cachedValue.copy(inFlight = Some(finalResult))
  }

  private def sendDelegateIfNeeded: CacheCommand => CachedValue[M, Res] = {
    case cacheCommand: NeedsToSendDelegateCacheCommand => sendDelegateFor(cacheCommand)
    case cacheCommand => cacheCommand.cachedValue
  }

  //  def updateLog = Functions.pipelineFn[CacheCommand](cc => logRequest(cc.req, cc.toString)) _
  //
  //  def updateMetrics = Functions.pipelineFn[CacheCommand](_.updateMetrics(metrics)) _

  private def cachedRequest(req: Req): M[Res] =
    map(cachableKey.id(req))(findCommand(req, staleState) _ ~^> (_.updateMetrics(metrics)) ~> sendDelegateIfNeeded).valueToUse


  override def apply(req: Req): M[Res]
  =
    if (cachableKey.bypassCache(req)) {
      metrics.bypassedRequests.incrementAndGet()
      raw(req)
    } else {
      metrics.requests.incrementAndGet()
      cachedRequest(req)
    }

  def findCommand(req: Req, staleState: CachedValue[M, Res] => StaleState)(c: CachedValue[M, Res]): CacheCommand = {
    (c, staleState(c)) match {
      case (c@CachedValue(_, _, None, _), Dead) => DeadNeedsClearingAndDelegation(req, c)
      case (c@CachedValue(_, _, Some(intransit), None), Dead) => DeadSomethingInTransit(req, c)
      case (c@CachedValue(__, _, None, None), _) => NoData(req, c)
      case (c@CachedValue(__, _, Some(inTransit), None), _) => HasInTransitDataOnly(req, c)
      case (c@CachedValue(_, _, _, Some(future)), Fresh) => HasFreshData(req, c)
      case (c@CachedValue(_, _, None, Some(future)), Stale) => HasStaleDataNeedsDelegation(req, c)
      case (c@CachedValue(_, _, Some(inTransit), _), Stale) => HasStaleDataSomethingInTransit(req, c)
      case (_, s) => UnexpectedState(req, c)
    }
  }
}


trait HasCachingCommands[M[_], Req, Res] {

  trait CacheCommand {
    def req: Req

    def cachedValue: CachedValue[M, Res]

    def updateMetrics(metrics: CachingMetrics) {}
  }

  trait NeedsToSendDelegateCacheCommand extends CacheCommand

  trait UpdateStaleMetrics extends CacheCommand {
    override def updateMetrics(metrics: CachingMetrics): Unit = metrics.staleRequest.incrementAndGet()
  }

  case class NoData(req: Req, cachedValue: CachedValue[M, Res]) extends NeedsToSendDelegateCacheCommand

  case class HasInTransitDataOnly(req: Req, cachedValue: CachedValue[M, Res]) extends CacheCommand

  case class HasFreshData(req: Req, cachedValue: CachedValue[M, Res]) extends CacheCommand

  case class HasStaleDataNeedsDelegation(req: Req, cachedValue: CachedValue[M, Res]) extends NeedsToSendDelegateCacheCommand with UpdateStaleMetrics

  case class HasStaleDataSomethingInTransit(req: Req, cachedValue: CachedValue[M, Res]) extends CacheCommand with UpdateStaleMetrics

  case class UnexpectedState(req: Req, cachedValue: CachedValue[M, Res]) extends CacheCommand

  case class DeadNeedsClearingAndDelegation(req: Req, rawCachedValue: CachedValue[M, Res]) extends NeedsToSendDelegateCacheCommand {
    val cachedValue = rawCachedValue.copy(value = None)

    override def updateMetrics(metrics: CachingMetrics): Unit = metrics.deadRequest.incrementAndGet()
  }

  case class DeadSomethingInTransit(req: Req, cachedValue: CachedValue[M, Res]) extends CacheCommand

}

