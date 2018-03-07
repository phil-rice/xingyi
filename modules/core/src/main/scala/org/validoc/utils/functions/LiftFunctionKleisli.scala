package org.validoc.utils.functions

import org.validoc.utils.language.Language._

import scala.language.higherKinds
import scala.reflect.ClassTag

trait LiftFunctionKleisli[M[_]] {
  protected implicit def monad: Monad[M]

  def function[Req: ClassTag, Res: ClassTag](name: String)(fn: Req => Res) = fn.liftFn

}
