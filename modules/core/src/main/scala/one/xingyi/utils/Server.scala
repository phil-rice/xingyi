package one.xingyi.utils

import scala.language.higherKinds
trait Closable[T] {
  def close(t: T)
}

