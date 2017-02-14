package org.validoc.utils.service

import org.validoc.utils.Service
import org.validoc.utils.aggregate.{EnrichParentChildService, Enricher, HasChildren, MergeService}
import org.validoc.utils.caching._
import org.validoc.utils.concurrency.Async
import org.validoc.utils.http._
import org.validoc.utils.map.MaxMapSizeStrategy
import org.validoc.utils.parser.ParserFinder
import org.validoc.utils.profiling.{ProfileOps, ProfilingService, TryProfileData}
import org.validoc.utils.retry.{NeedsRetry, RetryOps, RetryService}
import org.validoc.utils.time.{Delay, NanoTimeService}

import scala.concurrent.duration.{Duration, _}
import scala.reflect.ClassTag

case class StringServiceTag[Req, Res](t: String)

trait EndPoints[Tag[_, _]] {
  def endpoint0[Req: FromServiceRequest, Res: ToServiceResponse](path: String)(delegate: Tag[Req, Res]): Tag[ServiceRequest, ServiceResponse]

  def endpoint1[Req: FromServiceRequest, Res: ToServiceResponse](path: String)(delegate: Tag[Req, Res]): Tag[ServiceRequest, ServiceResponse]

  def endpoint2[Req: FromServiceRequest, Res: ToServiceResponse](path: String)(delegate: Tag[Req, Res]): Tag[ServiceRequest, ServiceResponse]

}


trait IService[Tag[_, _]] extends EndPoints[Tag] {
  def cached[Req: CachableKey, Res: CachableResult](timeToStale: Duration, timeToDead: Duration, maxSize: Int)(delegate: Tag[Req, Res])
                                                   (implicit timeService: NanoTimeService): Tag[Req, Res]

  def retry[Req, Res](delegate: Tag[Req, Res], resRetry: NeedsRetry[Res], retries: Int, delay: Delay)(implicit timeService: NanoTimeService): Tag[Req, Res]

  def profiled[Req, Res](delegate: Tag[Req, Res])(implicit timeService: NanoTimeService): Tag[Req, Res]



  def enrich[Req, Res, ResE, ReqC, ResC](parent: Tag[Req, Res],
                                         child: Tag[ReqC, ResC])
                                        (implicit
                                         enricher: Enricher[ResE, Res, ResC],
                                         children: HasChildren[Res, ReqC]): Tag[Req, ResE]

  def merge[ReqF, ResF, Req1, Res1, Req2, Res2](first: Tag[Req1, Res1],
                                                second: Tag[Req2, Res2],
                                                merger: (Res1, Res2) => ResF)
                                               (implicit reqMtoReq1: ReqF => Req1, reqMtoReq2: ReqF => Req2): Tag[ReqF, ResF]

  def aggregate[P, C](TagP: P, TagC: C) = (TagP, TagC)

  implicit class AggregatePimper[Req1, Res1, Req2, Res2](tuple: (Tag[Req1, Res1], Tag[Req2, Res2])) {
    def enrich[ResE](implicit enricher: Enricher[ResE, Res1, Res2], children: HasChildren[Res1, Req2]): Tag[Req1, ResE] =
      IService.this.enrich[Req1, Res1, ResE, Req2, Res2](tuple._1, tuple._2)

    def merge[ReqE, ResE](merger: (Res1, Res2) => ResE)(implicit reqMtoReq1: ReqE => Req1, reqMtoReq2: ReqE => Req2): Tag[ReqE, ResE] =
      IService.this.merge(tuple._1, tuple._2, merger)
  }


}


trait IHttpSetup[Tag[_, _], HttpReq, HttpRes] extends IService[Tag] {
  def rawService(name: String): Tag[HttpReq, HttpRes]
  def httpCallout[Req: ClassTag : ToServiceRequest, Res: ParserFinder : ClassTag](t: Tag[HttpReq, HttpRes])
                                                                                                   (implicit toServiceResponse: ToServiceResponse[HttpRes],
                                                                                                    toHttpReq: ServiceRequest => HttpReq): Tag[Req, Res]

  def getCachedProfiledObject[Req: ClassTag : ToServiceRequest : CachableKey, Res: ParserFinder : ClassTag : CachableResult]
  (timeToStale: Duration, timeToDead: Duration, maxSize: Int, rawService: Tag[HttpReq, HttpRes])
  (implicit toHttpReq: (ServiceRequest) => HttpReq, nanoTimeService: NanoTimeService, toHttpResponse: ToServiceResponse[HttpRes]): Tag[Req, Res] = {
    cached[Req, Res](timeToStale, timeToDead, maxSize)(profiled[Req, Res](httpCallout[Req, Res](rawService)))
  }
}


case class ServiceData[M[_], Req, Res, HttpReq, HttpRes](service: Req => M[Res],
                                                         endPoints: List[EndPointOps[M]] = List(),
                                                         cachedServices: List[CachingOps] = List(),
                                                         profileServices: List[ProfileOps] = List(),
                                                         retryServices: List[RetryOps] = List()) {
  def withService[ReqN, ResN](newService: ReqN => M[ResN]): ServiceData[M, ReqN, ResN, HttpReq, HttpRes] =
    ServiceData(newService, endPoints, cachedServices, profileServices, retryServices)

  def merge[ReqN, ResN](newService: ReqN => M[ResN], otherServiceData: ServiceData[M, _, _, HttpReq, HttpRes]): ServiceData[M, ReqN, ResN, HttpReq, HttpRes] =
    ServiceData(newService, endPoints ++ otherServiceData.endPoints,
      cachedServices ++ otherServiceData.cachedServices,
      profileServices ++ otherServiceData.profileServices,
      retryServices ++ otherServiceData.retryServices)
}

object ServiceInterpreters {

  class ServiceToString[HttpReq, HttpRes] extends IHttpSetup[StringServiceTag, HttpReq, HttpRes] {
    override def cached[Req: CachableKey, Res: CachableResult](timeToStale: Duration, timeToDead: Duration, maxSize: Int)(delegate: StringServiceTag[Req, Res])(implicit timeService: NanoTimeService): StringServiceTag[Req, Res] =
      StringServiceTag(s"Cached(${timeToStale}, ${timeToDead}, $maxSize) ~~~> ${delegate.t}")

    override def httpCallout[Req: ClassTag : ToServiceRequest,
    Res: ParserFinder : ClassTag](t: StringServiceTag[HttpReq, HttpRes])
                                                   (implicit toServiceResponse: ToServiceResponse[HttpRes],
                                                    toHttpReq: (ServiceRequest) => HttpReq): StringServiceTag[Req, Res] =
      StringServiceTag(s"Http(${implicitly[ClassTag[Req]].runtimeClass.getSimpleName},${implicitly[ClassTag[Res]].runtimeClass.getSimpleName}) ~~> ${t.t}")


    override def profiled[Req, Res](delegate: StringServiceTag[Req, Res])(implicit timeService: NanoTimeService): StringServiceTag[Req, Res] =
      StringServiceTag(s"Profile ~~> ${delegate.t}")

    override def rawService(name: String): StringServiceTag[HttpReq, HttpRes] = StringServiceTag(s"RawService($name)")

    override def enrich[Req, Res, ResE, ReqC, ResC](parent: StringServiceTag[Req, Res], child: StringServiceTag[ReqC, ResC])(implicit enricher: Enricher[ResE, Res, ResC], children: HasChildren[Res, ReqC]): StringServiceTag[Req, ResE] =
      StringServiceTag(s"Enrich(${parent.t}), ${child.t})")

    override def merge[ReqF, ResF, Req1, Res1, Req2, Res2](first: StringServiceTag[Req1, Res1], second: StringServiceTag[Req2, Res2], merger: (Res1, Res2) => ResF)(implicit reqMtoReq1: (ReqF) => Req1, reqMtoReq2: (ReqF) => Req2): StringServiceTag[ReqF, ResF] =
      StringServiceTag(s"Merge(${first.t}, ${second.t}")

    override def endpoint0[Req: FromServiceRequest, Res: ToServiceResponse](path: String)(delegate: StringServiceTag[Req, Res]): StringServiceTag[ServiceRequest, ServiceResponse] =
      StringServiceTag(s"endpoint0($path) ~~~> ${delegate.t}")

    override def endpoint1[Req: FromServiceRequest, Res: ToServiceResponse](path: String)(delegate: StringServiceTag[Req, Res]): StringServiceTag[ServiceRequest, ServiceResponse] =
      StringServiceTag(s"endpoint1($path) ~~~> ${delegate.t}")

    override def endpoint2[Req: FromServiceRequest, Res: ToServiceResponse](path: String)(delegate: StringServiceTag[Req, Res]): StringServiceTag[ServiceRequest, ServiceResponse] =
      StringServiceTag(s"endpoint2($path) ~~~> ${delegate.t}")

    //    override def retry[Req, Res](delegate: StringServiceTag[Req, Res], retryCount: Int, backOffPeriod: Duration)(implicit timeService: NanoTimeService): StringServiceTag[Req, Res] =
    override def retry[Req, Res](delegate: StringServiceTag[Req, Res], resRetry: NeedsRetry[Res], retries: Int, delay: Delay)(implicit timeService: NanoTimeService): StringServiceTag[Req, Res] =
      StringServiceTag(s"retry($retries), $delay) ~~~> ${delegate.t}")
  }

  class ServiceLanguageForAsync[M[_] : Async, HttpReq, HttpRes](services: Map[String, Service[M, HttpReq, HttpRes]]) {
    type AsyncService[Req, Res] = Req => M[Res]

    class ServiceToService extends IHttpSetup[AsyncService, HttpReq, HttpRes] {
      override def rawService(name: String): AsyncService[HttpReq, HttpRes] = services(name)

      override def cached[Req: CachableKey, Res: CachableResult](timeToStale: Duration, timeToDead: Duration, maxSize: Int)(delegate: Req => M[Res])(implicit timeService: NanoTimeService): AsyncService[Req, Res] =
        new CachingService[M, Req, Res]("someName", delegate, DurationStaleCacheStategy(timeToStale.toNanos, timeToDead.toNanos), MaxMapSizeStrategy(maxSize, maxSize / 4))

      override def retry[Req, Res](delegate: AsyncService[Req, Res], resRetry: NeedsRetry[Res], retries: Int, delay: Delay)(implicit timeService: NanoTimeService): AsyncService[Req, Res] =
        new RetryService[M, Req, Res](delegate, resRetry, retries, delay)

      override def profiled[Req, Res](delegate: Req => M[Res])(implicit timeService: NanoTimeService): AsyncService[Req, Res] =
        new ProfilingService[M, Req, Res]("someName", delegate, timeService)

      override def httpCallout[Req: ClassTag : ToServiceRequest, Res: ParserFinder : ClassTag](t: AsyncService[HttpReq, HttpRes])(implicit toServiceResponse: ToServiceResponse[HttpRes], toHttpReq: (ServiceRequest) => HttpReq): AsyncService[Req, Res] =
        new HttpObjectService[M, HttpReq, Req, HttpRes, Res]("someName", t, ResponseProcessor.parsed(implicitly[ParserFinder[Res]]))

      override def enrich[Req, Res, ResE, ReqC, ResC](parent: AsyncService[Req, Res], child: AsyncService[ReqC, ResC])(implicit enricher: Enricher[ResE, Res, ResC], children: HasChildren[Res, ReqC]): AsyncService[Req, ResE] =
        new EnrichParentChildService[M, Req, Res, ReqC, ResC, ResE](parent, child)

      override def merge[ReqF, ResF, Req1, Res1, Req2, Res2](first: AsyncService[Req1, Res1], second: AsyncService[Req2, Res2], merger: (Res1, Res2) => ResF)(implicit reqMtoReq1: (ReqF) => Req1, reqMtoReq2: (ReqF) => Req2): AsyncService[ReqF, ResF] =
        new MergeService[M, ReqF, ResF, Req1, Res1, Req2, Res2](first, second, merger)

      override def endpoint0[Req: FromServiceRequest, Res: ToServiceResponse](path: String)(delegate: AsyncService[Req, Res]): AsyncService[ServiceRequest, ServiceResponse] =
        new EndPointService[M, Req, Res](path, delegate)

      override def endpoint1[Req: FromServiceRequest, Res: ToServiceResponse](path: String)(delegate: AsyncService[Req, Res]): AsyncService[ServiceRequest, ServiceResponse] =
        new EndPointService[M, Req, Res](path, delegate)

      override def endpoint2[Req: FromServiceRequest, Res: ToServiceResponse](path: String)(delegate: AsyncService[Req, Res]): AsyncService[ServiceRequest, ServiceResponse] =
        new EndPointService[M, Req, Res](path, delegate)

    }

  }

  class ServicesGroupedForAsync[M[_] : Async, HttpReq, HttpRes](services: Map[String, Service[M, HttpReq, HttpRes]]) {
    type AsyncService[Req, Res] = Req => M[Res]
    type AsyncServiceData[Req, Res] = ServiceData[M, Req, Res, HttpReq, HttpRes]

    class ServiceToServiceData extends IHttpSetup[AsyncServiceData, HttpReq, HttpRes] {
      override def rawService(name: String): AsyncServiceData[HttpReq, HttpRes] =
        ServiceData(services(name))

      override def cached[Req: CachableKey, Res: CachableResult](timeToStale: Duration, timeToDead: Duration, maxSize: Int)(delegate: AsyncServiceData[Req, Res])(implicit timeService: NanoTimeService): AsyncServiceData[Req, Res] = {
        val newService = new CachingService[M, Req, Res]("someName", delegate.service, DurationStaleCacheStategy(timeToStale.toNanos, timeToDead.toNanos), MaxMapSizeStrategy(maxSize, maxSize / 4))
        delegate.withService(newService).copy(cachedServices = newService :: delegate.cachedServices)
      }

      override def retry[Req, Res](delegate: AsyncServiceData[Req, Res], resRetry: NeedsRetry[Res], retries: Int, delay: Delay)(implicit timeService: NanoTimeService): AsyncServiceData[Req, Res] = {
        val newService = new RetryService[M, Req, Res](delegate.service, resRetry, retries, delay)
        delegate.withService(newService).copy(retryServices = newService :: delegate.retryServices)
      }

      override def profiled[Req, Res](delegate: AsyncServiceData[Req, Res])(implicit timeService: NanoTimeService): AsyncServiceData[Req, Res] = {
        val newService = new ProfilingService[M, Req, Res]("someName", delegate.service, timeService)
        delegate.withService(newService).copy(profileServices = newService :: delegate.profileServices)
      }

      override def httpCallout[Req: ClassTag : ToServiceRequest, Res: ParserFinder : ClassTag](t: AsyncServiceData[HttpReq, HttpRes])(implicit toServiceResponse: ToServiceResponse[HttpRes], toHttpReq: (ServiceRequest) => HttpReq): AsyncServiceData[Req, Res] = {
        val newService: HttpObjectService[M, HttpReq, Req, HttpRes, Res] = new HttpObjectService[M, HttpReq, Req, HttpRes, Res]("someName", t.service, ResponseProcessor.parsed(implicitly[ParserFinder[Res]]))
        t.withService[Req, Res](newService)
      }

      override def enrich[Req, Res, ResE, ReqC, ResC](parent: AsyncServiceData[Req, Res], child: AsyncServiceData[ReqC, ResC])(implicit enricher: Enricher[ResE, Res, ResC], children: HasChildren[Res, ReqC]): AsyncServiceData[Req, ResE] = {
        val newService = new EnrichParentChildService[M, Req, Res, ReqC, ResC, ResE](parent.service, child.service)
        parent.merge(newService, child)
      }

      override def merge[ReqF, ResF, Req1, Res1, Req2, Res2](first: AsyncServiceData[Req1, Res1], second: AsyncServiceData[Req2, Res2], merger: (Res1, Res2) => ResF)(implicit reqMtoReq1: (ReqF) => Req1, reqMtoReq2: (ReqF) => Req2): AsyncServiceData[ReqF, ResF] = {
        val newService = new MergeService[M, ReqF, ResF, Req1, Res1, Req2, Res2](first.service, second.service, merger)
        first.merge(newService, second)
      }

      override def endpoint0[Req: FromServiceRequest, Res: ToServiceResponse](path: String)(delegate: AsyncServiceData[Req, Res]): AsyncServiceData[ServiceRequest, ServiceResponse] = {
        val newService = new EndPointService[M, Req, Res](path, delegate.service)
        delegate.withService(newService).copy(endPoints = newService :: delegate.endPoints)
      }

      override def endpoint1[Req: FromServiceRequest, Res: ToServiceResponse](path: String)(delegate: AsyncServiceData[Req, Res]): AsyncServiceData[ServiceRequest, ServiceResponse] = {
        val newService = new EndPointService[M, Req, Res](path, delegate.service)
        delegate.withService(newService).copy(endPoints = newService :: delegate.endPoints)
      }

      override def endpoint2[Req: FromServiceRequest, Res: ToServiceResponse](path: String)(delegate: AsyncServiceData[Req, Res]): AsyncServiceData[ServiceRequest, ServiceResponse] = {
        val newService = new EndPointService[M, Req, Res](path, delegate.service)
        delegate.withService(newService).copy(endPoints = newService :: delegate.endPoints)
      }
    }

  }

}