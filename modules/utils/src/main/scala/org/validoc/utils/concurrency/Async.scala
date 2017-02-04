package org.validoc.utils.concurrency

import org.validoc.utils.monads.{FlatMap, Monad}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/** M[_] is typically Future[_], or Task[_], or FutureEitherT or some other concurrency thing that does things in the future
  * Uses of M[_] are quite likely to make the assumption that Async.async fires things off in another thread.
  * */

trait Async[M[_]] extends Monad[M] {

  def async[T](t: => T): M[T]

  def delay(duration: FiniteDuration): M[Unit]

  def transform[T1, T2](mt: M[T1], fn: Try[T1] => M[T2]): M[T2]

  def registerSideEffectWhenComplete[T](m: M[T], sideEffect: Try[T] => Unit): M[T]
}



object Async {
  implicit def AsyncForFuture(implicit executionContext: ExecutionContext) = new Async[Future] {
    override def lift[T](t: => T): Future[T] = Try(t) match {
      case Success(s) => Future.successful(t)
      case Failure(e) => Future.failed(e)
    }
    override def async[T](t: => T): Future[T] = Future(t)

    override def flatMap[T, T2](m: Future[T], fn: (T) => Future[T2]): Future[T2] = m.flatMap[T2](fn)

    override def map[T, T2](m: Future[T], fn: (T) => T2): Future[T2] = m.map(fn)

    override def delay(duration: FiniteDuration): Future[Unit] =
      DelayedFuture(duration)(())

    override def registerSideEffectWhenComplete[T](m: Future[T], sideEffect: Try[T] => Unit): Future[T] =
      m.transformWith { tryT => sideEffect(tryT); m }

    override def liftTry[T](tryT: Try[T]): Future[T] = tryT.fold(Future.failed, Future.successful)

    override def transform[T1, T2](mt: Future[T1], fn: (Try[T1]) => Future[T2]): Future[T2] = mt.transformWith(fn)
  }

  //  implicit def AsyncWithFailureForFuture(implicit executionContext: ExecutionContext) = new AsyncWithFailure[Future, Throwable] {
  //    override def lift[T](t: T): Future[T] = Future.successful(t)
  //
  //    override def launch[T](t: => T): Future[T] = Future(t)
  //
  //    override def flatMap[T, T2](m: Future[T], fn: (T) => Future[T2]): Future[T2] = m.flatMap[T2](fn)
  //
  //    override def map[T, T2](m: Future[T], fn: (T) => T2): Future[T2] = m.map(fn)
  //
  //    override def liftFailure[T](f: Throwable): Future[T] = Future.failed(f)
  //
  //    override def onComplete[T1, T2](m: Future[T1], onSuccess: (T1) => Future[T2], onFailure: (Throwable) => Future[T2]): Future[T2] =
  //      m.transformWith(_ match {
  //        case Success(t) => onSuccess(t)
  //        case Failure(t) => onFailure(t)
  //      })
  //
  //    override def delay(duration: FiniteDuration): Future[Unit] =
  //      DelayedFuture(duration)(())
  //
  //    //    override def liftThrowable[T](throwable: Throwable): Future[T] = Future.failed(throwable)
  //
  //    override def registerSideEffectWhenComplete[T](m: Future[T], sideEffect: Try[T] => Unit): Future[T] =
  //      m.transformWith { tryT => sideEffect(tryT); m }
  //
  //    override def liftTry[T](tryT: Try[T]): Future[T] = tryT.fold(Future.failed, Future.successful)
  //  }

}

