package org.validoc.utils.metrics

import org.validoc.utils.concurrency.Async
import org.validoc.utils.time.NanoTimeService

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

trait MetricReporter {
  def apply[T: ReportData](name: String)(t: Try[T], duration: Long): Unit
}

class MetricsService[M[_]:Async, Req, Res: ReportData](name: String, report: MetricReporter)(delegate: Req => M[Res])(implicit  timeService: NanoTimeService) extends (Req => M[Res]) {
  override def apply(req: Req): M[Res] = timeService(delegate)(report[Res](name)).apply(req)
}
