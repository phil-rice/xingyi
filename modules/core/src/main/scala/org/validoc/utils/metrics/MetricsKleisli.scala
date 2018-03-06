package org.validoc.utils.metrics

import org.validoc.utils.functions.MonadCanFailWithException
import org.validoc.utils.time.NanoTimeService
import org.validoc.utils.language.Language._
import scala.reflect.ClassTag

trait MetricsKleisli[M[_], Fail] {
  protected implicit def monad: MonadCanFailWithException[M, Fail]
  protected def putMetrics: PutMetrics
  protected def timeService: NanoTimeService

  def metrics[Req: ClassTag, Res: ClassTag](prefix: String)(raw: Req => M[Res])(implicit rd: ReportData[Fail, Res]) =
    raw.onExit[Long](_ => timeService(), (duration, mRes) => mRes.onComplete(rd.apply(prefix, duration) ~> putMetrics))

}