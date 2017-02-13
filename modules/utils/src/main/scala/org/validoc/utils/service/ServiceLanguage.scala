package org.validoc.utils.service

import org.validoc.utils.Service
import org.validoc.utils.aggregate.{EnrichParentChildService, Enricher, HasChildren, MergeService}
import org.validoc.utils.caching.{CachableKey, CachableResult, CachingService, DurationStaleCacheStategy}
import org.validoc.utils.concurrency.Async
import org.validoc.utils.http.{ServiceRequest, ToServiceRequest, ToServiceResponse}
import org.validoc.utils.map.MaxMapSizeStrategy
import org.validoc.utils.parser.ParserFinder
import org.validoc.utils.time.NanoTimeService

import scala.concurrent.duration.{Duration, _}
import scala.reflect.ClassTag

case class StringServiceTag[ Req, Res](t: String)


trait IService[Tag[_, _]] {
  def cached[Req: CachableKey, Res: CachableResult](timeToStale: Duration, timeToDead: Duration, maxSize: Int)(delegate: Tag[Req, Res])
                                                   (implicit timeService: NanoTimeService): Tag[Req, Res]

  def profiled[Req, Res](delegate: Tag[Req, Res])(implicit timeService: NanoTimeService): Tag[Req, Res]

  def httpCallout[Req: ClassTag : ToServiceRequest, Res: ParserFinder : ClassTag, HttpReq, HttpRes](t: Tag[HttpReq, HttpRes])
                                                                                                   (implicit toServiceResponse: ToServiceResponse[HttpRes],
                                                                                                    toHttpReq: ServiceRequest => HttpReq): Tag[Req, Res]


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

  def endpoint0[Req, Res](path: String)(delegate: Tag[Req, Res])(implicit reqFn: ServiceRequest => Req): Tag[Req, Res]

  def endpoint1[Req, Res](path: String)(delegate: Tag[Req, Res])(implicit reqFn: (ServiceRequest, String) => Req): Tag[Req, Res]

  def endpoint2[Req, Res](path: String)(delegate: Tag[Req, Res])(implicit reqFn: (ServiceRequest, String, String) => Req): Tag[Req, Res]


}


trait IHttpSetup[Tag[_,_], HttpReq, HttpRes] extends IService[Tag] {
  def rawService(name: String): Tag[HttpReq, HttpRes]

  def getCachedProfiledObject[Req: ClassTag : ToServiceRequest : CachableKey, Res: ParserFinder : ClassTag : CachableResult]
  (timeToStale: Duration, timeToDead: Duration, maxSize: Int, rawService: Tag[HttpReq, HttpRes])
  (implicit toHttpReq: (ServiceRequest) => HttpReq, nanoTimeService: NanoTimeService, toHttpResponse: ToServiceResponse[HttpRes]): Tag[Req, Res] = {
    cached[Req, Res](timeToStale, timeToDead, maxSize)(profiled[Req, Res](httpCallout[Req, Res, HttpReq, HttpRes](rawService)))
  }

}


object ServiceInterpreters {

  class ServiceToString[HttpReq, HttpRes] extends IHttpSetup[StringServiceTag, HttpReq, HttpRes] {
    override def cached[Req: CachableKey, Res: CachableResult](timeToStale: Duration, timeToDead: Duration, maxSize: Int)(delegate: StringServiceTag[ Req, Res])(implicit timeService: NanoTimeService): StringServiceTag[ Req, Res] =
      StringServiceTag(s"Cached(${timeToStale}, ${timeToDead}, $maxSize) ~~~> ${delegate.t}")

    override def httpCallout[Req: ClassTag : ToServiceRequest,
    Res: ParserFinder : ClassTag, HttpReq, HttpRes](t: StringServiceTag[ HttpReq, HttpRes])
                                                   (implicit toServiceResponse: ToServiceResponse[HttpRes],
                                                    toHttpReq: (ServiceRequest) => HttpReq): StringServiceTag[ Req, Res] =
      StringServiceTag(s"Http(${implicitly[ClassTag[Req]].runtimeClass.getSimpleName},${implicitly[ClassTag[Res]].runtimeClass.getSimpleName}) ~~> ${t.t}")


    override def profiled[Req, Res](delegate: StringServiceTag[ Req, Res])(implicit timeService: NanoTimeService): StringServiceTag[ Req, Res] =
      StringServiceTag(s"Profile ~~> ${delegate.t}")

    override def rawService(name: String): StringServiceTag[ HttpReq, HttpRes] = StringServiceTag(s"RawService($name)")

    override def enrich[Req, Res, ResE, ReqC, ResC](parent: StringServiceTag[ Req, Res], child: StringServiceTag[ ReqC, ResC])(implicit enricher: Enricher[ResE, Res, ResC], children: HasChildren[Res, ReqC]): StringServiceTag[ Req, ResE] =
      StringServiceTag(s"Enrich(${parent.t}), ${child.t})")

    override def merge[ReqF, ResF, Req1, Res1, Req2, Res2](first: StringServiceTag[ Req1, Res1], second: StringServiceTag[ Req2, Res2], merger: (Res1, Res2) => ResF)(implicit reqMtoReq1: (ReqF) => Req1, reqMtoReq2: (ReqF) => Req2): StringServiceTag[ ReqF, ResF] =
      StringServiceTag(s"Merge(${first.t}, ${second.t}")

    override def endpoint0[Req, Res](path: String)(delegate: StringServiceTag[ Req, Res])(implicit reqFn: (ServiceRequest) => Req): StringServiceTag[ Req, Res] =
      StringServiceTag(s"endpoint0($path) ~~~> ${delegate.t}")

    override def endpoint1[Req, Res](path: String)(delegate: StringServiceTag[ Req, Res])(implicit reqFn: (ServiceRequest, String) => Req): StringServiceTag[ Req, Res] =
      StringServiceTag(s"endpoint1($path) ~~~> ${delegate.t}")

    override def endpoint2[Req, Res](path: String)(delegate: StringServiceTag[ Req, Res])(implicit reqFn: (ServiceRequest, String, String) => Req): StringServiceTag[ Req, Res] =
      StringServiceTag(s"endpoint2($path) ~~~> ${delegate.t}")
  }

}