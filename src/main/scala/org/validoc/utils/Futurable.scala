package org.validoc.utils

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}


/** M[_] is typically Future[_], or Task[_], or FutureEitherT or some other concurrency thing that does things in the future
  * Uses of M[_] are quite likely to make the assumption that launch fires things off in another thread.
  * */
trait Futurable[M[T]] {

  def lift[T](t: T): M[T]

  /** Users may make the assumption that the task done in launch is on another thread */
  def launch[T](t: => T): M[T]

  def map[T, T2](m: M[T], fn: T => T2): M[T2]

  def flatMap[T, T2](m: M[T], fn: T => M[T2]): M[T2]

  /** The parameter to onException might be a left or it might be an exception. Type safety is lost here because my type-fu isn't good enough to make Futurable nice to use and keep type safety */
  def onComplete[T](m: M[T], onSuccess: T => Unit, onFailure: Any => Unit): M[T]
}

object Futurable {

  implicit class MonadPimper[M[_] : Futurable, T](mt: M[T]) {
    def map[T2](fn: T => T2) = implicitly[Futurable[M]].map[T, T2](mt, fn)

    def flatMap[T2](fn: T => M[T2]) = implicitly[Futurable[M]].flatMap[T, T2](mt, fn)

    def onComplete[F](onSuccess: T => Unit, onFailure: Any => Unit) = implicitly[Futurable[M]].onComplete[T](mt, onSuccess, onFailure)
  }

  implicit def FuturableForFuture(implicit executionContext: ExecutionContext) = new Futurable[Future] {
    override def lift[T](t: T): Future[T] = Future.successful(t)

    override def launch[T](t: => T): Future[T] = Future(t)

    override def flatMap[T, T2](m: Future[T], fn: (T) => Future[T2]): Future[T2] = m.flatMap[T2](fn)

    override def map[T, T2](m: Future[T], fn: (T) => T2): Future[T2] = ???

    override def onComplete[T](m: Future[T], onSuccess: (T) => Unit, onFailure: (Any) => Unit) = {
      m.onComplete(_.fold(onFailure, onSuccess))
      m
    }
  }

}

