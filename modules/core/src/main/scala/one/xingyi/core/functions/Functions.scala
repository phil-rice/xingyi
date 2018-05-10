package one.xingyi.core.functions

import scala.language.higherKinds

object Functions {
  def print[P](msg: P => String): (P => P) = { p: P => println(msg(p)); p }

  def identify[X]: (X => X) = { x: X => x }

}
