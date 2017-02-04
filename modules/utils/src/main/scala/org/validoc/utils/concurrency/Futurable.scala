package org.validoc.utils.concurrency

import org.validoc.utils.monads.{FlatMap, Monad}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/** M[_] is typically Future[_], or Task[_], or FutureEitherT or some other concurrency thing that does things in the future
  * Uses of M[_] are quite likely to make the assumption that launch fires things off in another thread.
  * */


trait Futurable[M[_]] extends Monad[M] {

  /** Users may make the assumption that the task done in launch is on another thread */
  def launch[T](t: => T): M[T]

  def delay(duration: FiniteDuration): M[Unit]

  def registerSideEffectWhenComplete[T](m: M[T], sideEffect: Try[T] => Unit): M[T]
}


trait FuturableWithFailure[M[_], F] extends Futurable[M] {

  def liftFailure[T](f: F): M[T]

  def onComplete[T1, T2](m: M[T1], onSuccess: T1 => M[T2], onFailure: F => M[T2]): M[T2]

  def report[T](m: M[T], onSuccess: T => Unit, onFailure: F => Unit): M[T] =
    onComplete[T, T](m, { t => onSuccess(t); lift(t) }, { f => onFailure(f); liftFailure(f) })

}

object Futurable {


  implicit class MonadPimperWithFailures[M[_], T](mt: M[T]) {

    def liftFailure[F](f: F)(implicit futurableWithFailure: FuturableWithFailure[M, F]) = futurableWithFailure.liftFailure(f)


    def onComplete[T1, F](onSuccess: T => M[T1], onFailure: F => M[T1])(implicit futurableWithFailure: FuturableWithFailure[M, F]): M[T1] =
      futurableWithFailure.onComplete(mt, onSuccess, onFailure)

    def report[F](onSuccess: T => Unit, onFailure: F => Unit)(implicit futurableWithFailure: FuturableWithFailure[M, F]): M[T] =
      futurableWithFailure.report(mt, onSuccess, onFailure)
  }

  implicit def FuturableWithFailureForFuture(implicit executionContext: ExecutionContext) = new FuturableWithFailure[Future, Throwable] {
    override def lift[T](t: T): Future[T] = Future.successful(t)

    override def launch[T](t: => T): Future[T] = Future(t)

    override def flatMap[T, T2](m: Future[T], fn: (T) => Future[T2]): Future[T2] = m.flatMap[T2](fn)

    override def map[T, T2](m: Future[T], fn: (T) => T2): Future[T2] = m.map(fn)

    override def liftFailure[T](f: Throwable): Future[T] = Future.failed(f)

    override def onComplete[T1, T2](m: Future[T1], onSuccess: (T1) => Future[T2], onFailure: (Throwable) => Future[T2]): Future[T2] =
      m.transformWith(_ match {
        case Success(t) => onSuccess(t)
        case Failure(t) => onFailure(t)
      })

    override def delay(duration: FiniteDuration): Future[Unit] =
      DelayedFuture(duration)(())

    //    override def liftThrowable[T](throwable: Throwable): Future[T] = Future.failed(throwable)

    override def registerSideEffectWhenComplete[T](m: Future[T], sideEffect: Try[T] => Unit): Future[T] =
      m.transformWith { tryT => sideEffect(tryT); m }

    override def liftTry[T](tryT: Try[T]): Future[T] = tryT.fold(Future.failed, Future.successful)
  }

}

