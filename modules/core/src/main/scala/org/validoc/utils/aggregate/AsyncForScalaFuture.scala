package org.validoc.utils.aggregate

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}


class MultipleExceptions(val first: Throwable, val seq: List[Throwable]) extends Exception(s"Exceptions are\n ${(first :: seq).map(_.getClass.getName).mkString("\n")}", first)


object AsyncForScalaFuture {

  implicit def defaultAsyncForScalaFuture(implicit ec: ExecutionContext) = new AsyncMonad[Future] with WithFailure[Future, Throwable] {
    private def wrap[T](fn: Try[T] => Unit)(tryT: Try[T]): Try[T] = try {
      fn(tryT)
      tryT
    } catch {
      case e: Exception => tryT
    }
    override def respond[T](m: Future[T], fn: Try[T] => Unit): Future[T] = m.transform(wrap(fn))
    override def liftM[T](t: T): Future[T] = Future.successful(t)
    override def await[T](m: Future[T]): T = Await.result(m, 5 seconds)

    override def exception[T](t: Throwable) = Future.failed(t)
    override def map[T, T1](m: Future[T], fn: T => T1): Future[T1] = m.map(fn)
    override def flatMap[T, T1](m: Future[T], fn: T => Future[T1]): Future[T1] = m.flatMap(fn)
    override def onComplete[T](m: Future[T], fn: Try[Either[Throwable, T]] => Unit): Future[T] = m.transformWith {
      case Success(t) => fn(Success(Right(t))); Future.successful(t)
      case Failure(t) => fn(Success(Left(t))); Future.failed(t)
    }
    override def fail[T](f: Throwable): Future[T] = Future.failed(f)
    override def flattenM[T](seq: Seq[Future[T]]): Future[Seq[T]] = Future.sequence(seq)
  }

}
