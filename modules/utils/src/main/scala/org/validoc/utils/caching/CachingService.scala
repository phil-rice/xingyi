package org.validoc.utils.caching

import java.util.concurrent.atomic.AtomicLong

import org.validoc.utils.Service
import org.validoc.utils.concurrency.Async
import org.validoc.utils.functions.Functions
import org.validoc.utils.logging.Logging
import org.validoc.utils.map.{MapSizeStrategy, ReportMapSizeReduction, SafeMap}
import org.validoc.utils.time.NanoTimeService

import scala.language.higherKinds
import scala.util.Try


trait CachingOps {
  def name: String

  def clearCache

  def cachingMetrics: CachingMetricSnapShot
}

class CachingService[M[_], Req, Id, Res](val name: String,
                                         protected val delegate: Service[M, Req, Res],
                                         protected val cachingStrategy: CachingStrategy[M, Req, Id, Res],
                                         sizeStrategy: MapSizeStrategy)
                                        (implicit protected val async: Async[M])
  extends HasCachingCommands[M, Req, Id, Res] with Service[M, Req, Res] with CachingOps with Logging {

  import cachingStrategy._

  private val nextId = new AtomicLong()
  protected val metrics = CachingMetrics()
  protected val map = SafeMap[Id, CachedValue[M, Res]](CachedValue[M, Res](nanoTimeService(), CachedId(nextId.getAndIncrement()), None, None), sizeStrategy, metrics)

  override def clearCache = map.clear

  private def logRequest(request: Req, whatsHappening: String) = debug(s" Cache $name. $whatsHappening for request $request")

  private def staleState = cachingStrategy.staleCachingStrategy.state[M](name)(nanoTimeService) _

  override def cachingMetrics: CachingMetricSnapShot = metrics.snapshot(name, map, sizeStrategy.toString)

  private def recordResult(req: Req, c: CachedValue[M, Res])(tryRes: Try[Res]) = {
    trace(s"recordResult($req, $c)($tryRes)")
    map(id(req)) { cNew =>
      tryRes.fold(_ => metrics.delegateFailures, _ => metrics.delegateSuccesses).incrementAndGet()
      if (cNew.inFlightId != c.inFlightId) cNew
      else if (shouldCacheStrategy(tryRes)) cNew.copy(time = nanoTimeService(), inFlight = None, value = Some(async.liftTry(tryRes)))
      else cNew.copy(inFlight = None)
    }
  }

  private def sendDelegateIfNeeded: CacheCommand => CachedValue[M, Res] = {
    case cacheCommand: NeedsToSendDelegateCacheCommand => cacheCommand.cachedValue.copy(inFlight = Some(delegate(cacheCommand.req)))
    case cacheCommand => cacheCommand.cachedValue
  }

  def updateLog = Functions.pipelineFn[CacheCommand](cc => logRequest(cc.req, cc.toString)) _

  private def cachedRequest(req: Req): M[Res] = {

    map(id(req))(
      findCommand(req, staleState) _ andThen updateMetrics andThen updateLog andThen sendDelegateIfNeeded
    ).valueToUse
  }

  override def apply(req: Req): M[Res] =
    if (bypassCache(req)) {
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


  def updateMetrics = Functions.pipelineFn[CacheCommand](_.updateMetrics) _

}


trait HasCachingCommands[M[_], Req, Id, Res] extends Logging {
  protected def cachingStrategy: CachingStrategy[M, Req, Id, Res]

  protected def map: SafeMap[Id, CachedValue[M, Res]]

  protected def delegate: Service[M, Req, Res]

  protected def metrics: CachingMetrics

  protected implicit val async: Async[M]

  trait CacheCommand {
    def req: Req

    def cachedValue: CachedValue[M, Res]

    def updateMetrics {}
  }

  case class NoData(req: Req, initialCachedValue: CachedValue[M, Res]) extends NeedsToSendDelegateCacheCommand

  case class HasInTransitDataOnly(req: Req, cachedValue: CachedValue[M, Res]) extends CacheCommand

  case class HasFreshData(req: Req, cachedValue: CachedValue[M, Res]) extends CacheCommand

  case class HasStaleDataNeedsDelegation(req: Req, initialCachedValue: CachedValue[M, Res]) extends NeedsToSendDelegateCacheCommand with UpdateStaleMetrics

  case class HasStaleDataSomethingInTransit(req: Req, cachedValue: CachedValue[M, Res]) extends CacheCommand with UpdateStaleMetrics

  case class UnexpectedState(req: Req, cachedValue: CachedValue[M, Res]) extends CacheCommand

  case class DeadNeedsClearingAndDelegation(req: Req, rawCachedValue: CachedValue[M, Res]) extends NeedsToSendDelegateCacheCommand {
    val initialCachedValue = rawCachedValue.copy(value = None)

    override def updateMetrics: Unit = metrics.deadRequest.incrementAndGet()
  }

  case class DeadSomethingInTransit(req: Req, cachedValue: CachedValue[M, Res]) extends CacheCommand

  trait NeedsToSendDelegateCacheCommand extends CacheCommand {
    def req: Req

    def initialCachedValue: CachedValue[M, Res]

    private def recordResult(req: Req, c: CachedValue[M, Res])(tryRes: Try[Res]): CachedValue[M, Res] = {
      trace(s"recordResult($req, $c)($tryRes)")
      map(cachingStrategy.id(req)) { cNew =>
        tryRes.fold(_ => metrics.delegateFailures, _ => metrics.delegateSuccesses).incrementAndGet()
        if (cNew.inFlightId != c.inFlightId) cNew
        else if (cachingStrategy.shouldCacheStrategy(tryRes)) cNew.copy(time = cachingStrategy.nanoTimeService(), inFlight = None, value = Some(async.liftTry(tryRes)))
        else cNew.copy(inFlight = None)

      }
    }

    lazy val cachedValue = {
      metrics.delegateRequests.incrementAndGet()
      val delegateResult: M[Res] = delegate(req)
      val finalResult = implicitly[Async[M]].registerSideEffectWhenComplete(delegateResult, recordResult(req, initialCachedValue))
      initialCachedValue.copy(inFlight = Some(finalResult))
    }
  }

  trait UpdateStaleMetrics extends CacheCommand {
    override def updateMetrics: Unit = metrics.staleRequest.incrementAndGet()
  }

}