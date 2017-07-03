package org.validoc.language

import org.validoc.utils.aggregate.{Enricher, HasChildren}
import org.validoc.utils.caching.{CachableKey, CachableResult}
import org.validoc.utils.concurrency.Async
import org.validoc.utils.http._
import org.validoc.utils.metrics.{PutMetrics, ReportData}
import org.validoc.utils.parser.ParserFinder
import org.validoc.utils.retry.NeedsRetry
import org.validoc.utils.time.{Delay, NanoTimeService}
import org.validoc.utils.{FromServiceRequest, Service, ToServiceRequest, ToServiceResponse}

import scala.concurrent.duration.Duration
import scala.language.higherKinds
import scala.reflect.ClassTag


class MakeServiceInterpreter[M[_] : Async, HttpReq, HttpRes] extends MakeServices[Service, M, HttpReq, HttpRes] {

  override def rawService(hostName: HostName, port: Port)(implicit makeHttpService: MakeHttpService[M, HttpReq, HttpRes]): AsyncService[HttpReq, HttpRes] =
    makeHttpService.create(hostName, port)

  override def cached[Req: CachableKey, Res: CachableResult](timeToStale: Duration, timeToDead: Duration, maxSize: Int)(delegate: Req => M[Res])(implicit timeService: NanoTimeService): AsyncService[Req, Res] =
    makeCache(timeToStale, timeToDead, maxSize)(delegate)

  override def retry[Req, Res](delegate: AsyncService[Req, Res], resRetry: NeedsRetry[Res], retries: Int, delay: Delay)(implicit timeService: NanoTimeService): AsyncService[Req, Res] =
    makeRetry(delegate, resRetry, retries, delay)

  override def profiled[Req, Res](delegate: Req => M[Res])(implicit timeService: NanoTimeService): AsyncService[Req, Res] =
    makeProfile(delegate)

  override def httpCallout[Req: ClassTag : ToServiceRequest, Res: ParserFinder : ClassTag](t: AsyncService[HttpReq, HttpRes])(implicit toServiceResponse: ToServiceResponse[HttpRes], fromServiceRequest: FromServiceRequest[HttpReq]): AsyncService[Req, Res] =
    makeHhttpCallout(t)

  override def enrich[Req, Res, ResE, ReqC, ResC](parent: AsyncService[Req, Res], child: AsyncService[ReqC, ResC])(implicit enricher: Enricher[ResE, Res, ResC], children: HasChildren[Res, ReqC]): AsyncService[Req, ResE] =
    makeEnrich(parent, child)

  override def merge[ReqF, ResF, Req1, Res1, Req2, Res2](first: AsyncService[Req1, Res1], second: AsyncService[Req2, Res2], merger: (Res1, Res2) => ResF)(implicit reqMtoReq1: (ReqF) => Req1, reqMtoReq2: (ReqF) => Req2): AsyncService[ReqF, ResF] =
    makeMerge(first, second, merger)

  override def endpoint0[Req: FromServiceRequest, Res: ToServiceResponse](path: String)(delegate: AsyncService[Req, Res]): AsyncService[ServiceRequest, ServiceResponse] =
    makeEndpoint0(path)(delegate)

  override def endpoint1[Req: FromServiceRequest, Res: ToServiceResponse](path: String)(delegate: AsyncService[Req, Res]): AsyncService[ServiceRequest, ServiceResponse] =
    makeEndpoint1(path)(delegate)

  override def endpoint2[Req: FromServiceRequest, Res: ToServiceResponse](path: String)(delegate: AsyncService[Req, Res]): AsyncService[ServiceRequest, ServiceResponse] =
    makeEndpoint2(path)(delegate)

  override def metrics[Req, Res: ReportData](prefix: String)(delegate: Service[M, Req, Res])(implicit timeService: NanoTimeService, putMetrics: PutMetrics): Service[M, Req, Res] =
    makeMetrics(prefix)(delegate)

}
