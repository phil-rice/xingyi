package org.validoc.utils.service

import org.validoc.utils.Service
import org.validoc.utils.aggregate.{EnrichParentChildService, Enricher, HasChildren, MergeService}
import org.validoc.utils.caching._
import org.validoc.utils.concurrency.Async
import org.validoc.utils.http._
import org.validoc.utils.map.MaxMapSizeStrategy
import org.validoc.utils.parser.ParserFinder
import org.validoc.utils.profiling.{ProfileOps, ProfilingService}
import org.validoc.utils.retry.{NeedsRetry, RetryOps, RetryService}
import org.validoc.utils.time.{Delay, NanoTimeService}

import scala.concurrent.duration.Duration
import scala.reflect.ClassTag


case class StringServiceTag[M[_], Req, Res](t: String)


trait MakeHttpService[M[_], HttpReq, HttRes] {
  def create(name: String): (HttpReq => M[HttRes])
}

object MakeHttpService {
  def apply[M[_], HttpReq, HttpRes](map: Map[String, (HttpReq => M[HttpRes])]) = new MakeHttpService[M, HttpReq, HttpRes] {
    override def create(name: String): (HttpReq) => M[HttpRes] = map(name)
  }
}

trait EndPoints[Tag[M[_], _, _], M[_]] {
  def endpoint0[Req: FromServiceRequest, Res: ToServiceResponse](path: String)(delegate: Tag[M, Req, Res]): Tag[M, ServiceRequest, ServiceResponse]

  def endpoint1[Req: FromServiceRequest, Res: ToServiceResponse](path: String)(delegate: Tag[M, Req, Res]): Tag[M, ServiceRequest, ServiceResponse]

  def endpoint2[Req: FromServiceRequest, Res: ToServiceResponse](path: String)(delegate: Tag[M, Req, Res]): Tag[M, ServiceRequest, ServiceResponse]
}


trait IService[Tag[M[_], _, _], M[_]] extends EndPoints[Tag, M] {
  def cached[Req: CachableKey, Res: CachableResult](timeToStale: Duration, timeToDead: Duration, maxSize: Int)(delegate: Tag[M, Req, Res])
                                                   (implicit timeService: NanoTimeService): Tag[M, Req, Res]

  def retry[Req, Res](delegate: Tag[M, Req, Res], resRetry: NeedsRetry[Res], retries: Int, delay: Delay)(implicit timeService: NanoTimeService): Tag[M, Req, Res]

  def profiled[Req, Res](delegate: Tag[M, Req, Res])(implicit timeService: NanoTimeService): Tag[M, Req, Res]


  def enrich[Req, Res, ResE, ReqC, ResC](parent: Tag[M, Req, Res],
                                         child: Tag[M, ReqC, ResC])
                                        (implicit
                                         enricher: Enricher[ResE, Res, ResC],
                                         children: HasChildren[Res, ReqC]): Tag[M, Req, ResE]

  def merge[ReqF, ResF, Req1, Res1, Req2, Res2](first: Tag[M, Req1, Res1],
                                                second: Tag[M, Req2, Res2],
                                                merger: (Res1, Res2) => ResF)
                                               (implicit reqMtoReq1: ReqF => Req1, reqMtoReq2: ReqF => Req2): Tag[M, ReqF, ResF]

  def aggregate[P, C](TagP: P, TagC: C) = (TagP, TagC)

  implicit class AggregatePimper[Req1, Res1, Req2, Res2](tuple: (Tag[M, Req1, Res1], Tag[M, Req2, Res2])) {
    def enrich[ResE](implicit enricher: Enricher[ResE, Res1, Res2], children: HasChildren[Res1, Req2]): Tag[M, Req1, ResE] =
      IService.this.enrich[Req1, Res1, ResE, Req2, Res2](tuple._1, tuple._2)

    def merge[ReqE, ResE](merger: (Res1, Res2) => ResE)(implicit reqMtoReq1: ReqE => Req1, reqMtoReq2: ReqE => Req2): Tag[M, ReqE, ResE] =
      IService.this.merge(tuple._1, tuple._2, merger)
  }

}


trait IHttpSetup[Tag[M[_], _, _], M[_], HttpReq, HttpRes] extends IService[Tag, M] {
  def rawService(name: String)(implicit makeHttpService: MakeHttpService[M, HttpReq, HttpRes]): Tag[M, HttpReq, HttpRes]

  def httpCallout[Req: ClassTag : ToServiceRequest, Res: ParserFinder : ClassTag](t: Tag[M, HttpReq, HttpRes])
                                                                                 (implicit toServiceResponse: ToServiceResponse[HttpRes],
                                                                                  toHttpReq: ServiceRequest => HttpReq): Tag[M, Req, Res]

  def getCachedProfiledObject[Req: ClassTag : ToServiceRequest : CachableKey, Res: ParserFinder : ClassTag : CachableResult]
  (timeToStale: Duration, timeToDead: Duration, maxSize: Int, rawService: Tag[M, HttpReq, HttpRes])
  (implicit toHttpReq: (ServiceRequest) => HttpReq, nanoTimeService: NanoTimeService, toHttpResponse: ToServiceResponse[HttpRes]): Tag[M, Req, Res] = {
    cached[Req, Res](timeToStale, timeToDead, maxSize)(profiled[Req, Res](httpCallout[Req, Res](rawService)))
  }
}


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

object ServiceInterpreters {

  class ServiceToString[M[_], HttpReq, HttpRes] extends IHttpSetup[StringServiceTag, M, HttpReq, HttpRes] {
    override def cached[Req: CachableKey, Res: CachableResult](timeToStale: Duration, timeToDead: Duration, maxSize: Int)(delegate: StringServiceTag[M, Req, Res])(implicit timeService: NanoTimeService): StringServiceTag[M, Req, Res] =
      StringServiceTag(s"Cached(${timeToStale}, ${timeToDead}, $maxSize) ~~~> ${delegate.t}")

    override def httpCallout[Req: ClassTag : ToServiceRequest,
    Res: ParserFinder : ClassTag](t: StringServiceTag[M, HttpReq, HttpRes])
                                 (implicit toServiceResponse: ToServiceResponse[HttpRes],
                                  toHttpReq: (ServiceRequest) => HttpReq): StringServiceTag[M, Req, Res] =
      StringServiceTag(s"Http(${implicitly[ClassTag[Req]].runtimeClass.getSimpleName},${implicitly[ClassTag[Res]].runtimeClass.getSimpleName}) ~~> ${t.t}")


    override def profiled[Req, Res](delegate: StringServiceTag[M, Req, Res])(implicit timeService: NanoTimeService): StringServiceTag[M, Req, Res] =
      StringServiceTag(s"Profile ~~> ${delegate.t}")

    override def rawService(name: String)(implicit makeHttpService: MakeHttpService[M, HttpReq, HttpRes]): StringServiceTag[M, HttpReq, HttpRes] = StringServiceTag(s"RawService($name)")

    override def enrich[Req, Res, ResE, ReqC, ResC](parent: StringServiceTag[M, Req, Res], child: StringServiceTag[M, ReqC, ResC])(implicit enricher: Enricher[ResE, Res, ResC], children: HasChildren[Res, ReqC]): StringServiceTag[M, Req, ResE] =
      StringServiceTag(s"Enrich(${parent.t}), ${child.t})")

    override def merge[ReqF, ResF, Req1, Res1, Req2, Res2](first: StringServiceTag[M, Req1, Res1], second: StringServiceTag[M, Req2, Res2], merger: (Res1, Res2) => ResF)(implicit reqMtoReq1: (ReqF) => Req1, reqMtoReq2: (ReqF) => Req2): StringServiceTag[M, ReqF, ResF] =
      StringServiceTag(s"Merge(${first.t}, ${second.t}")

    override def endpoint0[Req: FromServiceRequest, Res: ToServiceResponse](path: String)(delegate: StringServiceTag[M, Req, Res]): StringServiceTag[M, ServiceRequest, ServiceResponse] =
      StringServiceTag(s"endpoint0($path) ~~~> ${delegate.t}")

    override def endpoint1[Req: FromServiceRequest, Res: ToServiceResponse](path: String)(delegate: StringServiceTag[M, Req, Res]): StringServiceTag[M, ServiceRequest, ServiceResponse] =
      StringServiceTag(s"endpoint1($path) ~~~> ${delegate.t}")

    override def endpoint2[Req: FromServiceRequest, Res: ToServiceResponse](path: String)(delegate: StringServiceTag[M, Req, Res]): StringServiceTag[M, ServiceRequest, ServiceResponse] =
      StringServiceTag(s"endpoint2($path) ~~~> ${delegate.t}")

    //    override def retry[Req, Res](delegate: StringServiceTag[Req, Res], retryCount: Int, backOffPeriod: Duration)(implicit timeService: NanoTimeService): StringServiceTag[Req, Res] =
    override def retry[Req, Res](delegate: StringServiceTag[M, Req, Res], resRetry: NeedsRetry[Res], retries: Int, delay: Delay)(implicit timeService: NanoTimeService): StringServiceTag[M, Req, Res] =
      StringServiceTag(s"retry($retries), $delay) ~~~> ${delegate.t}")
  }

  class ServiceLanguageForAsync[M[_] : Async, HttpReq, HttpRes] extends IHttpSetup[Service, M, HttpReq, HttpRes] {
    type AsyncService[Req, Res] = Req => M[Res]

    override def rawService(name: String)(implicit makeHttpService: MakeHttpService[M, HttpReq, HttpRes]): AsyncService[HttpReq, HttpRes] = makeHttpService.create(name)

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

  class ServicesGroupedForAsync[M[_] : Async, HttpReq, HttpRes] extends IHttpSetup[ServiceData, M, HttpReq, HttpRes] {

    //    def makeSetup: ServiceToServiceData = new ServiceToServiceData

    override def rawService(name: String)(implicit makeHttpService: MakeHttpService[M, HttpReq, HttpRes]): ServiceData[M, HttpReq, HttpRes] =
      ServiceData(makeHttpService.create(name))

    override def cached[Req: CachableKey, Res: CachableResult](timeToStale: Duration, timeToDead: Duration, maxSize: Int)(delegate: ServiceData[M, Req, Res])(implicit timeService: NanoTimeService): ServiceData[M, Req, Res] = {
      val newService = new CachingService[M, Req, Res]("someName", delegate.service, DurationStaleCacheStategy(timeToStale.toNanos, timeToDead.toNanos), MaxMapSizeStrategy(maxSize, maxSize / 4))
      delegate.withService(newService).copy(cachedServices = newService :: delegate.cachedServices)
    }

    override def retry[Req, Res](delegate: ServiceData[M, Req, Res], resRetry: NeedsRetry[Res], retries: Int, delay: Delay)(implicit timeService: NanoTimeService): ServiceData[M, Req, Res] = {
      val newService = new RetryService[M, Req, Res](delegate.service, resRetry, retries, delay)
      delegate.withService(newService).copy(retryServices = newService :: delegate.retryServices)
    }

    override def profiled[Req, Res](delegate: ServiceData[M, Req, Res])(implicit timeService: NanoTimeService): ServiceData[M, Req, Res] = {
      val newService = new ProfilingService[M, Req, Res]("someName", delegate.service, timeService)
      delegate.withService(newService).copy(profileServices = newService :: delegate.profileServices)
    }

    override def httpCallout[Req: ClassTag : ToServiceRequest, Res: ParserFinder : ClassTag](t: ServiceData[M, HttpReq, HttpRes])(implicit toServiceResponse: ToServiceResponse[HttpRes], toHttpReq: (ServiceRequest) => HttpReq): ServiceData[M, Req, Res] = {
      val newService: HttpObjectService[M, HttpReq, Req, HttpRes, Res] = new HttpObjectService[M, HttpReq, Req, HttpRes, Res]("someName", t.service, ResponseProcessor.parsed(implicitly[ParserFinder[Res]]))
      t.withService[Req, Res](newService)
    }

    override def enrich[Req, Res, ResE, ReqC, ResC](parent: ServiceData[M, Req, Res], child: ServiceData[M, ReqC, ResC])(implicit enricher: Enricher[ResE, Res, ResC], children: HasChildren[Res, ReqC]): ServiceData[M, Req, ResE] = {
      val newService = new EnrichParentChildService[M, Req, Res, ReqC, ResC, ResE](parent.service, child.service)
      parent.merge(newService, child)
    }

    override def merge[ReqF, ResF, Req1, Res1, Req2, Res2](first: ServiceData[M, Req1, Res1], second: ServiceData[M, Req2, Res2], merger: (Res1, Res2) => ResF)(implicit reqMtoReq1: (ReqF) => Req1, reqMtoReq2: (ReqF) => Req2): ServiceData[M, ReqF, ResF] = {
      val newService = new MergeService[M, ReqF, ResF, Req1, Res1, Req2, Res2](first.service, second.service, merger)
      first.merge(newService, second)
    }

    override def endpoint0[Req: FromServiceRequest, Res: ToServiceResponse](path: String)(delegate: ServiceData[M, Req, Res]): ServiceData[M, ServiceRequest, ServiceResponse] = {
      val newService = new EndPointService[M, Req, Res](path, delegate.service)
      delegate.withService(newService).copy(endPoints = newService :: delegate.endPoints)
    }

    override def endpoint1[Req: FromServiceRequest, Res: ToServiceResponse](path: String)(delegate: ServiceData[M, Req, Res]): ServiceData[M, ServiceRequest, ServiceResponse] = {
      val newService = new EndPointService[M, Req, Res](path, delegate.service)
      delegate.withService(newService).copy(endPoints = newService :: delegate.endPoints)
    }

    override def endpoint2[Req: FromServiceRequest, Res: ToServiceResponse](path: String)(delegate: ServiceData[M, Req, Res]): ServiceData[M, ServiceRequest, ServiceResponse] = {
      val newService = new EndPointService[M, Req, Res](path, delegate.service)
      delegate.withService(newService).copy(endPoints = newService :: delegate.endPoints)
    }
  }
}