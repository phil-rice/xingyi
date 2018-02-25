package org.validoc.utils.metrics

import org.validoc.utils._
import org.validoc.utils.functions.{MonadCanFail, MonadWithException}
import org.validoc.utils.time.NanoTimeService

import scala.language.higherKinds

trait PutMetrics extends (Map[String, MetricValue] => Unit)


object NullPutMetrics extends PutMetrics {
  override def apply(v1: Map[String, MetricValue]): Unit = ()
}

class MetricsService[M[_], Fail](implicit monad: MonadCanFail[M, Fail], timeService: NanoTimeService, putMetrics: PutMetrics) {

  def metrics[Req,Res](prefix: String, delegate: Req => M[Res])(implicit reportData: ReportData[Fail, Res]) =
    delegate.enterAndExit[Fail, Long]({ r: Req => timeService() }, reportData(prefix) ~> putMetrics)
}





