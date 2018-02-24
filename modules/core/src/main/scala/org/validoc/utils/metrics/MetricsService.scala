package org.validoc.utils.metrics

import org.validoc.utils.containers.Monad
import org.validoc.utils.time.NanoTimeService

import scala.language.higherKinds

trait PutMetrics extends (Map[String, MetricValue] => Unit)


object NullPutMetrics extends PutMetrics {
  override def apply(v1: Map[String, MetricValue]): Unit = ()
}

class MetricsService[M[_] : Monad, Req, Res](prefix: String, delegate: Req => M[Res])(implicit timeService: NanoTimeService, putMetrics: PutMetrics, reportData: ReportData[Res]) extends (Req => M[Res]) {
  override def apply(req: Req): M[Res] = timeService(delegate)((tryRes, duration) => putMetrics(reportData(prefix, tryRes, duration))) apply req
}


