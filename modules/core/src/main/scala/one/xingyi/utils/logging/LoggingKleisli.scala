package one.xingyi.utils.logging

import one.xingyi.utils.functions.MonadCanFailWithException
import one.xingyi.utils.language.Language._

import scala.language.higherKinds
import scala.reflect.ClassTag

trait LoggingKleisli[M[_], Fail] {
  protected implicit def monad: MonadCanFailWithException[M, Fail]
  protected def logReqAndResult: LogRequestAndResult[Fail]

  def logging[Req: ClassTag : DetailedLogging : SummaryLogging, Res: ClassTag : DetailedLogging : SummaryLogging](messagePrefix: String)(raw: Req => M[Res]) =
    raw.sideEffectWithReq[Fail](logReqAndResult[Req, Res](messagePrefix, messagePrefix))

}
