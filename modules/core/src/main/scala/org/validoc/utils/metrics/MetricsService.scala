package org.validoc.utils.metrics

import org.validoc.utils.concurrency.Async
import org.validoc.utils.service.{MakeServiceDescription, ServerContext, ServiceComposition}
import org.validoc.utils.time.NanoTimeService

import scala.language.higherKinds
import scala.reflect.ClassTag

trait PutMetrics extends (Map[String, MetricValue] => Unit)

object PutMetrics {
  implicit def putMetricsIfServerContextInScope(implicit serverContext: ServerContext[_, _]) = serverContext.putMetrics
}

object NullPutMetrics extends PutMetrics {
  override def apply(v1: Map[String, MetricValue]): Unit = ()
}

trait MetricsServiceLanguage[M[_]] extends ServiceComposition[M] {
  def metrics[Req: ClassTag, Res: ReportData : ClassTag](prefix: String)(implicit timeService: NanoTimeService, putMetrics: PutMetrics, async: Async[M]): MakeServiceDescription[M, Req, Res, Req, Res] =
    serviceWithParam[String, Req, Res, Req, Res, MetricsService[M, Req, Res]](prefix, {
      (prefix: String, delegate: Req => M[Res]) =>
        new MetricsService[M, Req, Res](prefix, delegate)
    })

}

class MetricsService[M[_] : Async, Req, Res](prefix: String, delegate: Req => M[Res])(implicit timeService: NanoTimeService, putMetrics: PutMetrics, reportData: ReportData[Res]) extends (Req => M[Res]) {
  override def apply(req: Req): M[Res] = timeService(delegate)((tryRes, duration) => putMetrics(reportData(prefix, tryRes, duration))) apply req
}


