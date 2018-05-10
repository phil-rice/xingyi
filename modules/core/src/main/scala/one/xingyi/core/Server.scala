package one.xingyi.core

import scala.language.higherKinds
trait Closable[T] {
  def close(t: T)
}

