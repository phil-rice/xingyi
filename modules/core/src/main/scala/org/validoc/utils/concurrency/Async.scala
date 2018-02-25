package org.validoc.utils.concurrency

import scala.language.higherKinds
import scala.util.Try

trait Async[M[_]] {
  def async[T](t: => T): M[T]
  def respond[T](m: M[T], fn: Try[T] => Unit): M[T]
  def await[T](m: M[T]): T
}




