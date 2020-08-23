package one.xingyi.core.language

import one.xingyi.core.monad.SuccessOrFail

object SuccessOrFailLanguage extends SuccessOrFailLanguage
trait SuccessOrFailLanguage {

  implicit class SuccessOfFailPimper[S[_], T](s: S[T])(implicit successOrFail: SuccessOrFail[S]) {
    def fold[Res](errorFn: Throwable => Res, fn: T => Res) = successOrFail.fold(s, errorFn, fn)
    def get = successOrFail.fold(s, e => throw e, (t: T) => t)
  }

}
