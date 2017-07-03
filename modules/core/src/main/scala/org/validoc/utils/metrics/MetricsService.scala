package org.validoc.utils.metrics

import org.validoc.utils.concurrency.Async
import org.validoc.utils.service.{MakeServiceMakerForClassWithParam, ServerContext}
import org.validoc.utils.time.NanoTimeService

import scala.language.higherKinds

trait PutMetrics extends (Map[String, MetricValue] => Unit)

object PutMetrics {
  implicit def putMetricsIfServerContextInScope(implicit serverContext: ServerContext[_, _]) = serverContext.putMetrics
}

object NullPutMetrics extends PutMetrics {
  override def apply(v1: Map[String, MetricValue]): Unit = ()
}


object MetricsService {
  implicit def makeMetricService[OldService <: Req => M[Res], M[_] : Async, Req, Res: ReportData](implicit timeService: NanoTimeService, putMetrics: PutMetrics) =
    new MakeServiceMakerForClassWithParam[String, OldService, MetricsService[M, Req, Res]] {
      override def apply(prefix: String, delegate: OldService): MetricsService[M, Req, Res] = new MetricsService[M, Req, Res](prefix, delegate)
    }

}

class MetricsService[M[_] : Async, Req, Res](prefix: String, delegate: Req => M[Res])(implicit timeService: NanoTimeService, putMetrics: PutMetrics, reportData: ReportData[Res]) extends (Req => M[Res]) {
  override def apply(req: Req): M[Res] = timeService(delegate)((tryRes, duration) => putMetrics(reportData(prefix, tryRes, duration))) apply req
}


