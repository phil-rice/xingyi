package org.validoc.utils.caching

import java.util.concurrent.atomic.AtomicLong

import org.validoc.utils.Service
import org.validoc.utils.concurrency.Async
import org.validoc.utils.functions.Functions
import org.validoc.utils.logging.Logging
import org.validoc.utils.map.{MapSizeStrategy, SafeMap}
import org.validoc.utils.time.NanoTimeService

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

trait Id

case class StringId(id: String) extends Id

case class IntId(id: String) extends Id

case object UnitId extends Id

trait CachableResult[Res] {
  def shouldCacheStrategy(req: Try[Res]): Boolean
}

trait CachableResultUsingSucesses[Res] extends CachableResult[Res] {
  def shouldCacheStrategy(req: Try[Res]): Boolean = req.isSuccess

}

object CachableResult {

  implicit object CachableResultForString extends CachableResultUsingSucesses[String]

}

trait CachableKey[Req] {
  def id(req: Req): Id

  def bypassCache(req: Req): Boolean
}

object CachableKey {

  implicit object CachableKeyForUnit extends CachableKey[Unit] {
    override def id(req: Unit): Id = UnitId

    override def bypassCache(req: Unit): Boolean = false
  }

}


trait CachingOps {
  def name: String

  def clearCache

  def cachingMetrics: CachingMetricSnapShot
}

class CachingService[M[_] : Async, Req: CachableKey, Res: CachableResult](val name: String,
                                                                          protected val delegate: Service[M, Req, Res],
                                                                          protected val cachingStrategy: StaleCacheStrategy,
                                                                          sizeStrategy: MapSizeStrategy)
                                                                         (implicit timeService: NanoTimeService)
  extends HasCachingCommands[M, Req, Res] with Service[M, Req, Res] with CachingOps with Logging {

  val async = implicitly[Async[M]]
  val cachableKey = implicitly[CachableKey[Req]]
  val cachableResult = implicitly[CachableResult[Res]]

  import cachingStrategy._

  private val nextId = new AtomicLong()
  protected val metrics = CachingMetrics()
  protected val map =
    SafeMap[Id, CachedValue[M, Res]](default = CachedValue[M, Res](timeService(), CachedId(nextId.getAndIncrement()), None, None), sizeStrategy, metrics)

  override def clearCache = map.clear

  private def logRequest(request: Req, whatsHappening: String) = debug(s" Cache $name. $whatsHappening for request $request")

  private def staleState = cachingStrategy.state[M](name) _

  override def cachingMetrics: CachingMetricSnapShot = metrics.snapshot(name, map, sizeStrategy.toString)

  private def recordResult(req: Req, c: CachedValue[M, Res])(tryRes: Try[Res]): CachedValue[M, Res] = {
    trace(s"recordResult($req, $c)($tryRes)")
    map(cachableKey.id(req)) { cNew =>
      (if (tryRes.isFailure) metrics.delegateFailures else metrics.delegateSuccesses).incrementAndGet()
      if (cNew.inFlightId != c.inFlightId) cNew
      else if (cachableResult.shouldCacheStrategy(tryRes)) cNew.copy(time = timeService(), inFlight = None, value = Some(async.liftTry(tryRes)))
      else cNew.copy(inFlight = None)
    }
  }

  private def sendDelegateFor(command: NeedsToSendDelegateCacheCommand) = {
    val cachedValue = command.cachedValue
    val req = command.req
    metrics.delegateRequests.incrementAndGet()
    val delegateResult: M[Res] = delegate(req)
    val finalResult = implicitly[Async[M]].registerSideEffectWhenComplete(delegateResult, recordResult(req, cachedValue))
    cachedValue.copy(inFlight = Some(finalResult))
  }

  private def sendDelegateIfNeeded: CacheCommand => CachedValue[M, Res] = {
    case cacheCommand: NeedsToSendDelegateCacheCommand => sendDelegateFor(cacheCommand)
    case cacheCommand => cacheCommand.cachedValue
  }

  def updateLog = Functions.pipelineFn[CacheCommand](cc => logRequest(cc.req, cc.toString)) _

  def updateMetrics = Functions.pipelineFn[CacheCommand](_.updateMetrics(metrics)) _

  private def cachedRequest(req: Req): M[Res] = {
    map(cachableKey.id(req))(
      findCommand(req, staleState) _ andThen updateMetrics andThen updateLog andThen sendDelegateIfNeeded
    ).valueToUse
  }

  override def apply(req: Req): M[Res] =
    if (cachableKey.bypassCache(req)) {
      metrics.bypassedRequests.incrementAndGet()
      delegate(req)
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


trait HasCachingCommands[M[_], Req, Res] extends Logging {

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