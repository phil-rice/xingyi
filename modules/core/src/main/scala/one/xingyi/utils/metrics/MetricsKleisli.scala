package one.xingyi.utils.metrics

import one.xingyi.utils.functions.MonadCanFailWithException
import one.xingyi.utils.time.NanoTimeService
import one.xingyi.utils.language.Language._
import scala.reflect.ClassTag
import scala.language.higherKinds

trait MetricsKleisli[M[_], Fail] {
  protected implicit def monad: MonadCanFailWithException[M, Fail]
  protected def putMetrics: PutMetrics
  protected def timeService: NanoTimeService

  def metrics[Req: ClassTag, Res: ClassTag](prefix: String)(raw: Req => M[Res])(implicit rd: ReportData[ Res]) =
    raw.onExit[Long](_ => timeService(), (duration, mRes) => mRes.onComplete[Fail](rd.apply(prefix, duration) ~> putMetrics))

}
