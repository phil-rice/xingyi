package org.validoc.language

import org.validoc.utils.aggregate.{Enricher, HasChildren}
import org.validoc.utils.caching.{CachableKey, CachableResult, CachingOps}
import org.validoc.utils.concurrency.Async
import org.validoc.utils.http.{ServiceRequest, ServiceResponse}
import org.validoc.utils.metrics.{PutMetrics, ReportData}
import org.validoc.utils.parser.ParserFinder
import org.validoc.utils.profiling.ProfileOps
import org.validoc.utils.retry.{NeedsRetry, RetryOps, RetryService}
import org.validoc.utils.service.EndPointOps
import org.validoc.utils.time.{Delay, NanoTimeService}
import org.validoc.utils.{FromServiceRequest, ToServiceRequest, ToServiceResponse}

import scala.concurrent.duration.Duration
import scala.reflect.ClassTag


case class ServiceData[M[_], Req, Res](service: Req => M[Res],
                                       endPoints: List[EndPointOps[M]] = List(),
                                       cachedServices: List[CachingOps] = List(),
                                       profileServices: List[ProfileOps] = List(),
                                       retryServices: List[RetryOps] = List()) {
  def withService[ReqN, ResN](newService: ReqN => M[ResN]): ServiceData[M, ReqN, ResN] =
    ServiceData(newService, endPoints, cachedServices, profileServices, retryServices)

  def merge[ReqN, ResN](newService: ReqN => M[ResN], otherServiceData: ServiceData[M, _, _]): ServiceData[M, ReqN, ResN] =
    ServiceData(newService, endPoints ++ otherServiceData.endPoints,
      cachedServices ++ otherServiceData.cachedServices,
      profileServices ++ otherServiceData.profileServices,
      retryServices ++ otherServiceData.retryServices)
}

class AggregatedServicesInterpreter[M[_] : Async, HttpReq, HttpRes] extends MakeServices[ServiceData, M, HttpReq, HttpRes] {

  override def rawService(name: String)(implicit makeHttpService: MakeHttpService[M, HttpReq, HttpRes]): ServiceData[M, HttpReq, HttpRes] =
    ServiceData(makeHttpService.create(name))

  override def cached[Req: CachableKey, Res: CachableResult](timeToStale: Duration, timeToDead: Duration, maxSize: Int)(delegate: ServiceData[M, Req, Res])(implicit timeService: NanoTimeService): ServiceData[M, Req, Res] = {
    val newService = makeCache(timeToStale, timeToDead, maxSize)(delegate.service)
    delegate.withService(newService).copy(cachedServices = newService :: delegate.cachedServices)
  }

  override def retry[Req, Res](delegate: ServiceData[M, Req, Res], resRetry: NeedsRetry[Res], retries: Int, delay: Delay)(implicit timeService: NanoTimeService): ServiceData[M, Req, Res] = {
    val newService: RetryService[M, Req, Res] = makeRetry(delegate.service, resRetry, retries, delay)
    delegate.withService(newService).copy(retryServices = newService :: delegate.retryServices)
  }

  override def profiled[Req, Res](delegate: ServiceData[M, Req, Res])(implicit timeService: NanoTimeService): ServiceData[M, Req, Res] = {
    val newService = makeProfile(delegate.service)
    delegate.withService(newService).copy(profileServices = newService :: delegate.profileServices)
  }

  override def httpCallout[Req: ClassTag : ToServiceRequest, Res: ParserFinder : ClassTag](t: ServiceData[M, HttpReq, HttpRes])(implicit toServiceResponse: ToServiceResponse[HttpRes], fromServiceRequest: FromServiceRequest[HttpReq]): ServiceData[M, Req, Res] = {
    t.withService[Req, Res](makeHhttpCallout[Req, Res](t.service))
  }

  override def enrich[Req, Res, ResE, ReqC, ResC](parent: ServiceData[M, Req, Res], child: ServiceData[M, ReqC, ResC])(implicit enricher: Enricher[ResE, Res, ResC], children: HasChildren[Res, ReqC]): ServiceData[M, Req, ResE] = {
    parent.merge(makeEnrich[Req, Res, ResE, ReqC, ResC](parent.service, child.service), child)
  }

  override def merge[ReqF, ResF, Req1, Res1, Req2, Res2](first: ServiceData[M, Req1, Res1], second: ServiceData[M, Req2, Res2], merger: (Res1, Res2) => ResF)(implicit reqMtoReq1: (ReqF) => Req1, reqMtoReq2: (ReqF) => Req2): ServiceData[M, ReqF, ResF] = {
    first.merge(makeMerge[ReqF, ResF, Req1, Res1, Req2, Res2](first.service, second.service, merger), second)
  }

  override def endpoint0[Req: FromServiceRequest, Res: ToServiceResponse](path: String)(delegate: ServiceData[M, Req, Res]): ServiceData[M, ServiceRequest, ServiceResponse] = {
    val newService = makeEndpoint0[Req, Res](path)(delegate.service)
    delegate.withService(newService).copy(endPoints = newService :: delegate.endPoints)
  }

  override def endpoint1[Req: FromServiceRequest, Res: ToServiceResponse](path: String)(delegate: ServiceData[M, Req, Res]): ServiceData[M, ServiceRequest, ServiceResponse] = {
    val newService = makeEndpoint1[Req, Res](path)(delegate.service)
    delegate.withService(newService).copy(endPoints = newService :: delegate.endPoints)
  }

  override def endpoint2[Req: FromServiceRequest, Res: ToServiceResponse](path: String)(delegate: ServiceData[M, Req, Res]): ServiceData[M, ServiceRequest, ServiceResponse] = {
    val newService = makeEndpoint2[Req, Res](path)(delegate.service)
    delegate.withService(newService).copy(endPoints = newService :: delegate.endPoints)
  }

  override def metrics[Req, Res: ReportData](prefix: String)(delegate: ServiceData[M, Req, Res])(implicit timeService: NanoTimeService, putMetrics: PutMetrics): ServiceData[M, Req, Res] = {
    val newService = makeMetrics[Req, Res](prefix)(delegate.service)
    delegate.withService[Req, Res](newService)
  }
}
