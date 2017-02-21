package org.validoc.utils.concurrency

import org.validoc.utils.monads.{FlatMap, Monad}
import sun.security.pkcs11.wrapper.Functions

import scala.concurrent.duration.{FiniteDuration, _}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/** M[_] is typically Future[_], or Task[_], or FutureEitherT or some other concurrency thing that does things in the future
  * Uses of M[_] are quite likely to make the assumption that Async.async fires things off in another thread.
  * */

trait Async[M[_]] extends Monad[M] {

  def async[T](t: => T): M[T]

  def delay(duration: FiniteDuration): M[Unit]

  def transform[T1, T2](mt: M[T1], fn: Try[T1] => M[T2]): M[T2]

  def registerSideEffectWhenComplete[T](m: M[T], sideEffect: Try[T] => _): M[T]

  def await[T](mt: M[T], duration: Duration): T
}

object Async {

  implicit class FlatMapPimper[M[_] : FlatMap, T](mt: M[T]) {
    def flatMap[T2](fn: T => M[T2]) = implicitly[FlatMap[M]].flatMap(mt, fn)
  }

  implicit class AsyncPimper[M[_], T](mt: M[T])(implicit async: Async[M]) {

    def join[T2](mt2: M[T2]) = async.join2(mt, mt2)

    def map[T2](fn: T => T2) = async.map(mt, fn)

    def transform[T2](fn: Try[T] => M[T2]) = async.transform(mt, fn)

    def registerSideEffectWhenComplete[T2](sideEffect: Try[T] => Unit) = async.registerSideEffectWhenComplete(mt, sideEffect)
  }

  implicit class SeqOfMsPimper[M[_] : Async, T](seq: Seq[M[T]]) {
    def join: M[Seq[T]] = implicitly[Async[M]].join(seq)
  }

  implicit class AsyncFunctionPimper[M[_], P, R](fn: P => M[R])(implicit async: Async[M]) {
    def andTransform[R2](fail: Throwable => M[R2], succeed: R => M[R2]) = { p: P =>
      async.transform(fn(p), { tryR: Try[R] =>
        tryR match {
          case Success(s) => succeed(s)
          case Failure(t) => fail(t)
        }
      })
    }

    def andTransformAndLift[R2](fail: Throwable => R2, succeed: R => R2) = { p: P =>
      async.transform(fn(p), { tryR: Try[R] =>
        tryR match {
          case Success(s) => succeed(s).lift
          case Failure(t) => fail(t).lift
        }
      })
    }
  }

  implicit class ValuePimper[T](t: T) {
    def lift[M[_] : Async]: M[T] = implicitly[Async[M]].lift(t)

    def liftValue[M[_] : Async]: M[T] = implicitly[Async[M]].lift(t)
  }

  implicit class TryPimper[T](t: Try[T]) {
    def liftTry[M[_] : Async]: M[T] = implicitly[Async[M]].liftTry(t)
  }

  implicit class ExceptionPimper(t: Throwable) {
    def liftThrowable[M[_] : Async, T]: M[T] = implicitly[Async[M]].liftTry(Failure(t))
  }


  implicit def asyncForFuture(implicit executionContext: ExecutionContext) = new Async[Future] {
    override def lift[T](t: => T): Future[T] = Try(t) match {
      case Success(s) => Future.successful(t)
      case Failure(e) => Future.failed(e)
    }

    override def async[T](t: => T): Future[T] = Future(t)

    override def flatMap[T, T2](m: Future[T], fn: (T) => Future[T2]): Future[T2] = m.flatMap[T2](fn)

    override def map[T, T2](m: Future[T], fn: (T) => T2): Future[T2] = m.map(fn)

    override def delay(duration: FiniteDuration): Future[Unit] =
      DelayedFuture(duration)(())

    override def registerSideEffectWhenComplete[T](m: Future[T], sideEffect: Try[T] => _): Future[T] = {
      m.onComplete { tryT: Try[T] => sideEffect(tryT) }
      m
    }

    //      m.transformWith { tryT => sideEffect(tryT); m }

    override def liftTry[T](tryT: Try[T]): Future[T] = tryT match {
      case Success(t) => Future.successful(t)
      case Failure(t) => Future.failed(t)
    }

    override def transform[T1, T2](mt: Future[T1], fn: (Try[T1]) => Future[T2]): Future[T2] =
      mt.flatMap[T2](t => fn(Success(t))).recoverWith { case t: Throwable => fn(Failure(t)) }

    override def await[T](mt: Future[T], duration: Duration): T = Await.result(mt, duration)
  }

}

