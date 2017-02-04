package org.validoc.utils.monads

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

trait FlatMap[M[_]] {
  def flatMap[T, T2](m: M[T], fn: T => M[T2]): M[T2]
}

object FlatMap {
  implicit def FlatMapForFuture(implicit executionContext: ExecutionContext) = new FlatMap[Future] {
    override def flatMap[A, B](a: Future[A], fn: (A) => Future[B]): Future[B] = a.flatMap(fn)
  }
}

trait Monad[M[_]] extends FlatMap[M] {
  def lift[T](t: => T): M[T]

  /** This may just throw the exception if it's not meaningfull to lift it. Meaningful monads include Try, Future ... */
  def liftTry[T](tryT: Try[T]): M[T]

  def map[T, T2](m: M[T], fn: T => T2): M[T2]

  def flatMap[T, T2](m: M[T], fn: T => M[T2]): M[T2]
}

object Monad {

  implicit class MonadPimper[M[_] : Monad, T](mt: M[T]) {
    def map[T2](fn: T => T2) = implicitly[Monad[M]].map[T, T2](mt, fn)

    def flatMap[T2](fn: T => M[T2]) = implicitly[Monad[M]].flatMap[T, T2](mt, fn)

  }

}

