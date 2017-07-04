package org.validoc.language

import org.validoc.utils.aggregate.{Enricher, HasChildren}
import org.validoc.utils.caching._
import org.validoc.utils.functions.Functions._
import org.validoc.utils.http._
import org.validoc.utils.metrics.{PutMetrics, ReportData}
import org.validoc.utils.parser.ParserFinder
import org.validoc.utils.retry.NeedsRetry
import org.validoc.utils.success.Succeeded
import org.validoc.utils.time.{Delay, NanoTimeService}

import scala.concurrent.duration.Duration
import scala.language.higherKinds
import scala.reflect.ClassTag

case class StringServiceTag[M[_], Req, Res](t: String)



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

  def metrics[Req, Res: ReportData](prefix: String)(delegate: Tag[M, Req, Res])(implicit timeService: NanoTimeService, putMetrics: PutMetrics): Tag[M, Req, Res]

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
  def rawService(hostName: HostName, port: Port)(implicit makeHttpService: MakeHttpService[M, HttpReq, HttpRes]): Tag[M, HttpReq, HttpRes]

  def httpCallout[Req: ClassTag : ToServiceRequest, Res: ParserFinder : ClassTag](t: Tag[M, HttpReq, HttpRes])
                                                                                 (implicit toServiceResponse: ToServiceResponse[HttpRes],
                                                                                  fromServiceRequest: FromServiceRequest[HttpReq]): Tag[M, Req, Res]

  def getCachedProfiledObject[Req: ClassTag : ToServiceRequest : CachableKey, Res: ParserFinder : ClassTag : CachableResult]
  (metricPrefix: String, timeToStale: Duration, timeToDead: Duration, maxSize: Int, rawService: Tag[M, HttpReq, HttpRes])
  (implicit fromServiceRequest: FromServiceRequest[HttpReq], nanoTimeService: NanoTimeService, toHttpResponse: ToServiceResponse[HttpRes], putMetrics: PutMetrics, succeeded: Succeeded[HttpRes]): Tag[M, Req, Res] = {
    (metrics[HttpReq, HttpRes](metricPrefix) _ ~> httpCallout[Req, Res] _ ~> profiled ~> cached(timeToStale, timeToDead, maxSize) _) (rawService)
  }

}



