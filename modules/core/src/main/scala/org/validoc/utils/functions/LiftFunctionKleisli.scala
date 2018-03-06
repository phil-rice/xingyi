package org.validoc.utils.functions

import org.validoc.utils.language.Language._

trait LiftFunctionKleisli[M[_]] {
  protected implicit def monad: Monad[M]

  def function[Req, Res](name: String)(fn: Req => Res) = fn.liftFn

}
