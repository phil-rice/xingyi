package org.validoc.language

import org.validoc.utils.aggregate.{Enricher, HasChildren}
import org.validoc.utils.caching.{CachableKey, CachableResult}
import org.validoc.utils.http.{ServiceRequest, ServiceResponse}
import org.validoc.utils.metrics.{PutMetrics, ReportData}
import org.validoc.utils.parser.ParserFinder
import org.validoc.utils.retry.NeedsRetry
import org.validoc.utils.time.{Delay, NanoTimeService}
import org.validoc.utils.{FromServiceRequest, ToServiceRequest, ToServiceResponse}

import scala.concurrent.duration.Duration
import scala.language.higherKinds
import scala.reflect.ClassTag

object ToStringInterpreter {
  def apply[M[_], HttpReq, HttpRes]() = new ToStringInterpreter[M, HttpReq, HttpRes]
}

class ToStringInterpreter[M[_], HttpReq, HttpRes] extends IHttpSetup[StringServiceTag, M, HttpReq, HttpRes] {
  override def cached[Req: CachableKey, Res: CachableResult](timeToStale: Duration, timeToDead: Duration, maxSize: Int)(delegate: StringServiceTag[M, Req, Res])(implicit timeService: NanoTimeService): StringServiceTag[M, Req, Res] =
    StringServiceTag(s"Cached($timeToStale, $timeToDead, $maxSize) ~~~> ${delegate.t}")

  override def httpCallout[Req: ClassTag : ToServiceRequest,
  Res: ParserFinder : ClassTag](t: StringServiceTag[M, HttpReq, HttpRes])
                               (implicit toServiceResponse: ToServiceResponse[HttpRes],
                                fromServiceRequest: FromServiceRequest[HttpReq]): StringServiceTag[M, Req, Res] =
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

  override def metrics[Req, Res: ReportData](prefix: String)(delegate: StringServiceTag[M, Req, Res])(implicit timeService: NanoTimeService, putMetrics: PutMetrics): StringServiceTag[M, Req, Res] =
    StringServiceTag(s"metrics ~~~> ${delegate.t}")
}