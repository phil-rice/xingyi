package org.validoc.language

import org.validoc.utils.aggregate.{EnrichParentChildService, Enricher, HasChildren, MergeService}
import org.validoc.utils.caching.{CachableKey, CachableResult, CachingService, DurationStaleCacheStategy}
import org.validoc.utils.concurrency.Async
import org.validoc.utils.http.{HttpObjectService, ResponseProcessor}
import org.validoc.utils.map.MaxMapSizeStrategy
import org.validoc.utils.metrics.{MetricsService, PutMetrics, ReportData}
import org.validoc.utils.parser.ParserFinder
import org.validoc.utils.profiling.ProfilingService
import org.validoc.utils.retry.{NeedsRetry, RetryService}
import org.validoc.utils.service.{EndPointOps, EndPointService}
import org.validoc.utils.time.{Delay, NanoTimeService}
import org.validoc.utils.{FromServiceRequest, Service, ToServiceRequest, ToServiceResponse}

import scala.concurrent.duration.Duration
import scala.language.higherKinds
import scala.reflect.ClassTag

abstract class MakeServices[Tag[M[_], _, _], M[_] : Async, HttpReq, HttpRes] extends IHttpSetup[Tag, M, HttpReq, HttpRes] {
  type AsyncService[Req, Res] = Req => M[Res]

  def makeCache[Req: CachableKey, Res: CachableResult](timeToStale: Duration, timeToDead: Duration, maxSize: Int)(delegate: Req => M[Res])(implicit timeService: NanoTimeService) =
    new CachingService[M, Req, Res]("someName", delegate, DurationStaleCacheStategy(timeToStale.toNanos, timeToDead.toNanos), MaxMapSizeStrategy(maxSize, maxSize / 4))

  def makeRetry[Req, Res](delegate: AsyncService[Req, Res], resRetry: NeedsRetry[Res], retries: Int, delay: Delay)(implicit timeService: NanoTimeService) =
    new RetryService[M, Req, Res](delegate, resRetry, retries, delay)

  def makeProfile[Req, Res](delegate: Req => M[Res])(implicit timeService: NanoTimeService) =
    new ProfilingService[M, Req, Res]("someName", delegate, timeService)

  def makeHhttpCallout[Req: ClassTag : ToServiceRequest, Res: ParserFinder : ClassTag](t: AsyncService[HttpReq, HttpRes])(implicit toServiceResponse: ToServiceResponse[HttpRes], fromServiceRequest: FromServiceRequest[HttpReq]): AsyncService[Req, Res] =
    new HttpObjectService[M, HttpReq, Req, HttpRes, Res]("someName", t, ResponseProcessor.parsed(implicitly[ParserFinder[Res]]))

  def makeEnrich[Req, Res, ResE, ReqC, ResC](parent: AsyncService[Req, Res], child: AsyncService[ReqC, ResC])(implicit enricher: Enricher[ResE, Res, ResC], children: HasChildren[Res, ReqC]): AsyncService[Req, ResE] =
    new EnrichParentChildService[M, Req, Res, ReqC, ResC, ResE](parent, child)

  def makeMerge[ReqF, ResF, Req1, Res1, Req2, Res2](first: AsyncService[Req1, Res1], second: AsyncService[Req2, Res2], merger: (Res1, Res2) => ResF)(implicit reqMtoReq1: (ReqF) => Req1, reqMtoReq2: (ReqF) => Req2): AsyncService[ReqF, ResF] =
    new MergeService[M, ReqF, ResF, Req1, Res1, Req2, Res2](first, second, merger)

  def makeEndpoint0[Req: FromServiceRequest, Res: ToServiceResponse](path: String)(delegate: AsyncService[Req, Res]): EndPointOps[M] =
    new EndPointService[M, Req, Res](path, delegate)

  def makeEndpoint1[Req: FromServiceRequest, Res: ToServiceResponse](path: String)(delegate: AsyncService[Req, Res]): EndPointOps[M] =
    new EndPointService[M, Req, Res](path, delegate)

  def makeEndpoint2[Req: FromServiceRequest, Res: ToServiceResponse](path: String)(delegate: AsyncService[Req, Res]): EndPointOps[M] =
    new EndPointService[M, Req, Res](path, delegate)

  def makeMetrics[Req, Res: ReportData](prefix: String)(delegate: Service[M, Req, Res])(implicit timeService: NanoTimeService, putMetrics: PutMetrics): Service[M, Req, Res] =
    new MetricsService[M, Req, Res](prefix, delegate)


}







