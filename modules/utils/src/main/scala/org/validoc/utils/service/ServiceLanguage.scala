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

case class ServiceTag[T, Req, Res](t: T)


trait IService[T] {
  def cached[Req: CachableKey, Res: CachableResult](timeToStale: Duration, timeToDead: Duration, maxSize: Int)(delegate: ServiceTag[T, Req, Res])
                                                   (implicit timeService: NanoTimeService): ServiceTag[T, Req, Res]

  def profiled[Req, Res](delegate: ServiceTag[T, Req, Res])(implicit timeService: NanoTimeService): ServiceTag[T, Req, Res]

  def httpCallout[Req: ClassTag : ToServiceRequest, Res: ParserFinder : ClassTag, HttpReq, HttpRes](t: ServiceTag[T, HttpReq, HttpRes])
                                                                                                   (implicit toServiceResponse: ToServiceResponse[HttpRes],
                                                                                                    toHttpReq: ServiceRequest => HttpReq): ServiceTag[T, Req, Res]


  def enrich[Req, Res, ResE, ReqC, ResC](parent: ServiceTag[T, Req, Res],
                                         child: ServiceTag[T, ReqC, ResC])
                                        (implicit
                                         enricher: Enricher[ResE, Res, ResC],
                                         children: HasChildren[Res, ReqC]): ServiceTag[T, Req, ResE]

  def merge[ReqF, ResF, Req1, Res1, Req2, Res2](first: ServiceTag[T, Req1, Res1],
                                                second: ServiceTag[T, Req2, Res2],
                                                merger: (Res1, Res2) => ResF)
                                               (implicit reqMtoReq1: ReqF => Req1, reqMtoReq2: ReqF => Req2): ServiceTag[T, ReqF, ResF]

  def aggregate[P, C](serviceTagP: P, serviceTagC: C) = (serviceTagP, serviceTagC)

  implicit class AggregatePimper[Req1, Res1, Req2, Res2](tuple: (ServiceTag[T, Req1, Res1], ServiceTag[T, Req2, Res2])) {
    def enrich[ResE](implicit enricher: Enricher[ResE, Res1, Res2], children: HasChildren[Res1, Req2]): ServiceTag[T, Req1, ResE] =
      IService.this.enrich[Req1, Res1, ResE, Req2, Res2](tuple._1, tuple._2)

    def merge[ReqE, ResE](merger: (Res1, Res2) => ResE)(implicit reqMtoReq1: ReqE => Req1, reqMtoReq2: ReqE => Req2): ServiceTag[T, ReqE, ResE] =
      IService.this.merge(tuple._1, tuple._2, merger)
  }

  def endpoint0[Req, Res](path: String)(delegate: ServiceTag[T, Req, Res])(implicit reqFn: ServiceRequest => Req): ServiceTag[T, Req, Res]

  def endpoint1[Req, Res](path: String)(delegate: ServiceTag[T, Req, Res])(implicit reqFn: (ServiceRequest, String) => Req): ServiceTag[T, Req, Res]

  def endpoint2[Req, Res](path: String)(delegate: ServiceTag[T, Req, Res])(implicit reqFn: (ServiceRequest, String, String) => Req): ServiceTag[T, Req, Res]


}


trait IHttpSetup[T, HttpReq, HttpRes] extends IService[T] {
  def rawService(name: String): ServiceTag[T, HttpReq, HttpRes]

  def getCachedProfiledObject[Req: ClassTag : ToServiceRequest : CachableKey, Res: ParserFinder : ClassTag : CachableResult]
  (timeToStale: Duration, timeToDead: Duration, maxSize: Int, rawService: ServiceTag[T, HttpReq, HttpRes])
  (implicit toHttpReq: (ServiceRequest) => HttpReq, nanoTimeService: NanoTimeService, toHttpResponse: ToServiceResponse[HttpRes]): ServiceTag[T, Req, Res] = {
    cached[Req, Res](timeToStale, timeToDead, maxSize)(profiled[Req, Res](httpCallout[Req, Res, HttpReq, HttpRes](rawService)))
  }

}


object ServiceInterpreters {

  class ServiceToString[HttpReq, HttpRes] extends IHttpSetup[String, HttpReq, HttpRes] {
    override def cached[Req: CachableKey, Res: CachableResult](timeToStale: Duration, timeToDead: Duration, maxSize: Int)(delegate: ServiceTag[String, Req, Res])(implicit timeService: NanoTimeService): ServiceTag[String, Req, Res] =
      ServiceTag(s"Cached(${timeToStale}, ${timeToDead}, $maxSize) ~~~> ${delegate.t}")

    override def httpCallout[Req: ClassTag : ToServiceRequest,
    Res: ParserFinder : ClassTag, HttpReq, HttpRes](t: ServiceTag[String, HttpReq, HttpRes])
                                                   (implicit toServiceResponse: ToServiceResponse[HttpRes],
                                                    toHttpReq: (ServiceRequest) => HttpReq): ServiceTag[String, Req, Res] =
      ServiceTag(s"Http(${implicitly[ClassTag[Req]].runtimeClass.getSimpleName},${implicitly[ClassTag[Res]].runtimeClass.getSimpleName}) ~~> ${t.t}")


    override def profiled[Req, Res](delegate: ServiceTag[String, Req, Res])(implicit timeService: NanoTimeService): ServiceTag[String, Req, Res] =
      ServiceTag(s"Profile ~~> ${delegate.t}")

    override def rawService(name: String): ServiceTag[String, HttpReq, HttpRes] = ServiceTag(s"RawService($name)")

    override def enrich[Req, Res, ResE, ReqC, ResC](parent: ServiceTag[String, Req, Res], child: ServiceTag[String, ReqC, ResC])(implicit enricher: Enricher[ResE, Res, ResC], children: HasChildren[Res, ReqC]): ServiceTag[String, Req, ResE] =
      ServiceTag(s"Enrich(${parent.t}), ${child.t})")

    override def merge[ReqF, ResF, Req1, Res1, Req2, Res2](first: ServiceTag[String, Req1, Res1], second: ServiceTag[String, Req2, Res2], merger: (Res1, Res2) => ResF)(implicit reqMtoReq1: (ReqF) => Req1, reqMtoReq2: (ReqF) => Req2): ServiceTag[String, ReqF, ResF] =
      ServiceTag(s"Merge(${first.t}, ${second.t}")

    override def endpoint0[Req, Res](path: String)(delegate: ServiceTag[String, Req, Res])(implicit reqFn: (ServiceRequest) => Req): ServiceTag[String, Req, Res] =
      ServiceTag(s"endpoint0($path) ~~~> ${delegate.t}")

    override def endpoint1[Req, Res](path: String)(delegate: ServiceTag[String, Req, Res])(implicit reqFn: (ServiceRequest, String) => Req): ServiceTag[String, Req, Res] =
      ServiceTag(s"endpoint1($path) ~~~> ${delegate.t}")

    override def endpoint2[Req, Res](path: String)(delegate: ServiceTag[String, Req, Res])(implicit reqFn: (ServiceRequest, String, String) => Req): ServiceTag[String, Req, Res] =
      ServiceTag(s"endpoint2($path) ~~~> ${delegate.t}")
  }

  abstract class AsService[M[_] : Async, HttpReq, HttpRes] extends IHttpSetup[Service[M, _, _], HttpReq, HttpRes] {
    override def cached[Req: CachableKey, Res: CachableResult](timeToStale: Duration, timeToDead: Duration, maxSize: Int)(delegate: ServiceTag[Service[M, _, _], Req, Res])(implicit timeService: NanoTimeService): ServiceTag[Service[M, _, _], Req, Res] =
      ServiceTag(new CachingService[M, Req, Res]("someName",
        delegate.t.asInstanceOf[Service[M, Req, Res]],
        DurationStaleCacheStategy(timeToStale.toNanos, timeToDead.toNanos),
        MaxMapSizeStrategy(maxSize, maxSize / 4)))


    //    override def profiled[Req, Res](delegate: ServiceTag[Service[M, _, _], Req, Res])(implicit timeService: NanoTimeService): ServiceTag[Service[M, _, _], Req, Res] = ???
    //
    //    override def httpCallout[Req: ClassManifest : ToServiceRequest, Res: ParserFinder : ClassManifest, HttpReq, HttpRes](t: ServiceTag[Service[M, _, _], HttpReq, HttpRes])(implicit toServiceResponse: ToServiceResponse[HttpRes], toHttpReq: (ServiceRequest) => HttpReq): ServiceTag[Service[M, _, _], Req, Res] = ???
    //
    //    override def enrich[Req, Res, ResE, ReqC, ResC](parent: ServiceTag[Service[M, _, _], Req, Res], child: ServiceTag[Service[M, _, _], ReqC, ResC])(implicit enricher: Enricher[ResE, Res, ResC], children: HasChildren[Res, ReqC]): ServiceTag[Service[M, _, _], Req, ResE] = ???
    //
    //    override def merge[ReqF, ResF, Req1, Res1, Req2, Res2](first: ServiceTag[Service[M, _, _], Req1, Res1], second: ServiceTag[Service[M, _, _], Req2, Res2], merger: (Res1, Res2) => ResF)(implicit reqMtoReq1: (ReqF) => Req1, reqMtoReq2: (ReqF) => Req2): ServiceTag[Service[M, _, _], ReqF, ResF] = ???
    //
    //    override def endpoint0[Req, Res](path: String)(delegate: ServiceTag[Service[M, _, _], Req, Res])(implicit reqFn: (ServiceRequest) => Req): ServiceTag[Service[M, _, _], Req, Res] = ???
    //
    //    override def endpoint1[Req, Res](path: String)(delegate: ServiceTag[Service[M, _, _], Req, Res])(implicit reqFn: (ServiceRequest, String) => Req): ServiceTag[Service[M, _, _], Req, Res] = ???
    //
    //
    //    override def endpoint2[Req, Res](path: String)(delegate: ServiceTag[Service[M, _, _], Req, Res])(implicit reqFn: (ServiceRequest, String, String) => Req): ServiceTag[Service[M, _, _], Req, Res] = ???
  }

}