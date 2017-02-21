package org.validoc.utils.success

import scala.util.Try


trait Succeeded[T] {
  def apply(t: Try[T]): SucceededState[T]
}

sealed trait SucceededState[T]{
  def asKey: String
}

case class SuccessState[T](t: T) extends SucceededState[T] {
  override def asKey: String = "success"
}

case class FailedState[T](t: T) extends SucceededState[T] {
  override def asKey: String = "failure"
}

case class ExceptionState[T](t: Throwable) extends SucceededState[T] {
  override def asKey: String = "exception"
}
