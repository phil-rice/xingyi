package org.validoc.utils.service

import org.validoc.utils.caching.{CachableKey, CachableResult}
import org.validoc.utils.http.{ServiceRequest, ToServiceRequest, ToServiceResponse}
import org.validoc.utils.parser.ParserFinder
import org.validoc.utils.time.NanoTimeService

import scala.concurrent.duration.{Duration, _}
import scala.reflect.ClassTag

case class ServiceTag[T, Req, Res](t: T)


trait IService[T] {
  def cached[Req: CachableKey, Res: CachableResult](timeToStale: Duration, timeToDead: Duration, maxSize: Int)( delegate: ServiceTag[T, Req, Res]): ServiceTag[T, Req, Res]

  def profiled[Req, Res](delegate: ServiceTag[T, Req, Res])(implicit timeService: NanoTimeService): ServiceTag[T, Req, Res]

  def httpCallout[Req: ClassTag : ToServiceRequest, Res: ParserFinder : ClassTag, HttpReq, HttpRes](t: ServiceTag[T, HttpReq, HttpRes])
                                                                                                   (implicit toServiceResponse: ToServiceResponse[HttpRes],
                                                                                                    toHttpReq: ServiceRequest => HttpReq): ServiceTag[T, Req, Res]


  def enrich[Req, Res, ResE, ReqC, ResC](parent: ServiceTag[T, Req, Res],
                                         child: ServiceTag[T, ReqC, ResC],
                                         childReqs: Res => Seq[ReqC],
                                         enricher: (Res, Seq[ResC]) => ResE): ServiceTag[T, Req, ResE]

  def merge[ReqF, ResF, Req1, Res1, Req2, Res2](first: ServiceTag[T, Req1, Res1],
                                                second: ServiceTag[T, Req2, Res2],
                                                firstReq: ReqF => Req1,
                                                secondReq: ReqF => Req2,
                                                merger: (Res1, Res2) => ResF): ServiceTag[T, ReqF, ResF]
}


trait IHttpSetup[T, HttpReq, HttpRes] extends IService[T] {
  def rawService(name: String): ServiceTag[T, HttpReq, HttpRes]
}


object ServiceInterpreters {

  class ServiceToString[HttpReq, HttpRes] extends IHttpSetup[String, HttpReq, HttpRes] {
    override def cached[Req: CachableKey, Res: CachableResult](timeToStale: Duration, timeToDead: Duration, maxSize: Int)( delegate: ServiceTag[String, Req, Res]): ServiceTag[String, Req, Res] =
      ServiceTag(s"Cached(${timeToStale}, ${timeToDead}, $maxSize) ~~~> ${delegate.t}")

    override def httpCallout[Req: ClassTag : ToServiceRequest, Res: ParserFinder : ClassTag, HttpReq, HttpRes](t: ServiceTag[String, HttpReq, HttpRes])
                                                                                                              (implicit toServiceResponse: ToServiceResponse[HttpRes],
                                                                                                               toHttpReq: (ServiceRequest) => HttpReq): ServiceTag[String, Req, Res] =
      ServiceTag(s"Http(${implicitly[ClassTag[Req]].runtimeClass.getSimpleName},${implicitly[ClassTag[Res]].runtimeClass.getSimpleName}) ~~> ${t.t}")


    override def profiled[Req, Res](delegate: ServiceTag[String, Req, Res])(implicit timeService: NanoTimeService): ServiceTag[String, Req, Res] =
      ServiceTag(s"Profile ~~> ${delegate.t}")

    override def enrich[Req, Res, ResE, ReqC, ResC](parent: ServiceTag[String, Req, Res], child: ServiceTag[String, ReqC, ResC], childReqs: (Res) => Seq[ReqC], enricher: (Res, Seq[ResC]) => ResE): ServiceTag[String, Req, ResE] =
      ServiceTag(s"Enrich(${parent.t}), ${child.t})")

    override def merge[ReqF, ResF, Req1, Res1, Req2, Res2](first: ServiceTag[String, Req1, Res1], second: ServiceTag[String, Req2, Res2], firstReq: (ReqF) => Req1, secondReq: (ReqF) => Req2, merger: (Res1, Res2) => ResF) =
      ServiceTag(s"Merge(${first.t}, ${second.t}")

    override def rawService(name: String): ServiceTag[String, HttpReq, HttpRes] = ServiceTag(s"RawService($name)")
  }

}