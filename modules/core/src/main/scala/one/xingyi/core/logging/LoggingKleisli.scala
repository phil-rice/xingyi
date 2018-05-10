package one.xingyi.core.logging

import one.xingyi.core.functions.MonadCanFailWithException
import one.xingyi.core.language.Language._

import scala.language.higherKinds
import scala.reflect.ClassTag

trait LoggingKleisli[M[_], Fail] {
  protected implicit def monad: MonadCanFailWithException[M, Fail]
  protected def logReqAndResult: LogRequestAndResult[Fail]

  def logging[Req: ClassTag : DetailedLogging : SummaryLogging, Res: ClassTag : DetailedLogging : SummaryLogging](messagePrefix: String)(raw: Req => M[Res]) =
    raw.sideEffectWithReq[Fail](logReqAndResult[Req, Res](messagePrefix, messagePrefix))

}
