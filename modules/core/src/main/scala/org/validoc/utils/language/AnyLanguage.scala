package org.validoc.utils.language

import org.validoc.utils.functions._

import scala.util.Try
import scala.language.higherKinds

object AnyLanguage extends AnyLanguage
trait AnyLanguage {

  def withValue[X, Y](x: X)(fn: X => Y) = fn(x)

  def sideeffect[X, Y](x: X)(fn: X => Y) = {
    fn(x)
    x
  }
  implicit class AnyPimper[T](t: T) {
    def |>[T2](fn: T => T2) = fn(t)
    def liftM[M[_]](implicit container: Liftable[M]): M[T] = container.liftM(t)
    def |+>[T1](fn: T => T => T1): T1 = fn(t)(t)
    def liftException[M[_], T1](implicit async: MonadWithException[M], ev: T <:< Throwable): M[T1] = async.exception(t)
    def |?[M[_] : Functor, Failure](validation: T => Seq[Failure])(implicit withFailure: MonadCanFail[M, Failure], monoid: Monoid[Failure]): M[T] =
      validation(t) match {
        case Nil => t.liftM
        case s => withFailure.fail(monoid.addAll(s))
      }
  }

  implicit class BooleanPimper(boolean: Boolean) {
    def toOption[T](value: => T) = if (boolean) Some(value) else None
  }

  implicit class EitherPimper[L, R](either: Either[L, R]) {
    def liftEither[M[_]](implicit monad: MonadCanFail[M, L]): M[R] = either.fold(monad.fail(_), monad.liftM(_))
    def getOrException(exceptionCreator: L => Throwable): R =
      either match {
        case Right(r) => r
        case Left(l) => throw exceptionCreator(l)
      }
  }

  implicit class TryPimper[T](tryT: Try[T]) {
    def liftTry[M[_]](implicit monadWithException: MonadWithException[M]) = monadWithException.liftTry(tryT)
  }

}
