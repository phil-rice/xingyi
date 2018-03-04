package org.validoc.utils.logging

import org.validoc.utils._
import org.validoc.utils.functions.MonadCanFailWithException
import org.validoc.utils.tagless.CommonForKleislis

import scala.language.higherKinds
import scala.reflect.ClassTag

trait LoggingKleisli[M[_], Fail] extends CommonForKleislis[M] {
  implicit def monad: MonadCanFailWithException[M, Fail]
  protected val logReqAndResult: LogRequestAndResult[Fail]

  def logging[Req: ClassTag : DetailedLogging : SummaryLogging, Res: ClassTag : DetailedLogging : SummaryLogging](messagePrefix: String)(raw: Kleisli[Req, Res]) =
    raw.sideEffectWithReq[Fail](logReqAndResult[Req, Res](messagePrefix, messagePrefix))

}
