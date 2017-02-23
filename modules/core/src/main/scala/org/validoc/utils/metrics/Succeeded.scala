package org.validoc.utils.metrics

import scala.util.{Failure, Success, Try}

trait Succeeded[T] {
  def apply(t: Try[T]): SucceededState[T]
}


class DefaultSucceeded[T] extends Succeeded[T] {
  def apply(t: Try[T]) = t match {
    case Success(t) => SuccessState(t)
    case Failure(t) => ExceptionState(t)
  }
}

class SucceededFromFn[T](ok: T => Boolean) extends Succeeded[T] {
  def apply(t: Try[T]) = t match {
    case Success(t) if ok(t) => SuccessState(t)
    case Success(t) => FailedState(t)
    case Failure(t) => ExceptionState(t)
  }
}

object Succeeded {
  implicit def succeededFromEither[L, R] = new SucceededFromEither[L, R]
}

class SucceededFromEither[L, R] extends Succeeded[Either[L, R]] {
  override def apply(t: Try[Either[L, R]]): SucceededState[Either[L, R]] = t match {
    case Success(Right(s)) => SuccessState(Right(s))
    case Success(Left(f)) => FailedState(Left(f))
    case Failure(t) => ExceptionState(t)
  }
}

sealed trait SucceededState[T] {
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