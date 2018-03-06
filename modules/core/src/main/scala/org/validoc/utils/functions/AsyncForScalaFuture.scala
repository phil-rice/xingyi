package org.validoc.utils.functions

import java.util.concurrent.TimeUnit

import org.validoc.utils.concurrency.DelayedFuture
import org.validoc.utils.local.{ExecutionContextWithLocal, LocalOpsForScalaFuture}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

class MultipleExceptions(val first: Throwable, val seq: List[Throwable]) extends Exception(s"Exceptions are\n ${(first :: seq).map(_.getClass.getName).mkString("\n")}", first)


class AsyncForScalaFuture(implicit ex: ExecutionContextWithLocal) extends Async[Future] with MonadCanFailWithException[Future, Throwable] {
  private def wrap[T](fn: Try[T] => Unit)(tryT: Try[T]): Try[T] = try {
    fn(tryT)
    tryT
  } catch {
    case e: Exception => tryT
  }
  override def foldWithExceptionAndFail[T, T1](m: Future[T], fnE: Throwable => Future[T1], fnFailure: Throwable => Future[T1], fn: T => Future[T1]) = m.transformWith(_ match {
    case Success(t) => fn(t)
    case Failure(t) => fnE(t)
  })
  override def recover[T](m: Future[T], fn: Throwable => Future[T]) = m.recoverWith { case e: Throwable => fn(e) }

  override def respond[T](m: Future[T], fn: Try[T] => Unit): Future[T] = m.transform(wrap(fn))
  override def liftM[T](t: T): Future[T] = Future.successful(t)
  override def await[T](m: Future[T]): T = Await.result(m, 5 seconds)

  override def exception[T](t: Throwable) = Future.failed(t)
  override def map[T, T1](m: Future[T], fn: T => T1): Future[T1] = m.map(fn)
  override def flatMap[T, T1](m: Future[T], fn: T => Future[T1]): Future[T1] = m.flatMap(fn)
  override def fail[T](f: Throwable): Future[T] = Future.failed(f)
  override def flattenM[T](seq: Seq[Future[T]]): Future[Seq[T]] = Future.sequence(seq)
  override def async[T](t: => T) = Future(t)
  override def delay[T](duration: Duration)(block: => Future[T]): Future[T] = DelayedFuture(duration)(block)
  override def mapEither[T, T1](m: Future[T], fn: Either[Throwable, T] => Future[T1]): Future[T1] = m.transformWith {
    case Success(t) => fn(Right(t))
    case Failure(t) => fn(Left(t))
  }

}

class HadUnexpectedFailException(f: Any) extends Exception(f.toString)

class AsyncForScalaFutureEither[Fail] {
  type FutureEither[T] = Future[Either[Fail, T]]

  class AsyncForFutureEither(implicit ex: ExecutionContextWithLocal) extends MonadCanFailWithException[FutureEither, Fail] with Async[FutureEither] {
    override def mapEither[T, T1](m: FutureEither[T], fn: Either[Fail, T] => FutureEither[T1]): FutureEither[T1] = m.flatMap(fn)
    override def async[T](t: => T): FutureEither[T] = Future(Right(t))
    override def respond[T](m: FutureEither[T], fn: Try[T] => Unit): FutureEither[T] = m.transformWith {
      _ match {
        case Success(Right(t)) => fn(Success(t)); liftM(t)
        case Success(Left(f)) => fail(f)
        case Failure(t) => fn(Failure(t)); exception(t)
      }
    }
    override def await[T](m: FutureEither[T]): T = Await.result(m, Duration(5, TimeUnit.SECONDS)).fold(f => throw new HadUnexpectedFailException(f), Functions.identify)
    override def delay[T](duration: Duration)(block: => FutureEither[T]): FutureEither[T] = DelayedFuture(duration)(block)
    override def flatMap[T, T1](m: FutureEither[T], fn: T => FutureEither[T1]): FutureEither[T1] = {
      m.flatMap {
        case Right(t) => fn(t)
        case Left(f) => Future.successful(Left(f))
      }
    }

    override def map[T, T1](m: FutureEither[T], fn: T => T1): FutureEither[T1] = m.map(_.map(fn))
    override def liftM[T](t: T): FutureEither[T] = Future.successful(Right(t))
    override def fail[T](f: Fail): FutureEither[T] = Future.successful(Left(f))
    override def recover[T](m: FutureEither[T], fn: Throwable => FutureEither[T]): FutureEither[T] = m.recoverWith { case e => fn(e) }
    override def foldWithExceptionAndFail[T, T1](m: FutureEither[T], fnE: Throwable => FutureEither[T1], fnFailure: Fail => FutureEither[T1], fn: T => FutureEither[T1]): FutureEither[T1] = m.transformWith {
      case Success(Right(t)) => fn(t)
      case Success(Left(t)) => fnFailure(t)
      case Failure(t) => fnE(t)
    }
    override def exception[T](t: Throwable): FutureEither[T] = Future.failed(t)
  }

}

object AsyncForScalaFuture {

  object ImplicitsForTest {
    implicit val executionContext = new ExecutionContextWithLocal(ExecutionContext.Implicits.global)
  }

  implicit def defaultAsyncForScalaFuture(implicit ec: ExecutionContextWithLocal) = new AsyncForScalaFuture

}


