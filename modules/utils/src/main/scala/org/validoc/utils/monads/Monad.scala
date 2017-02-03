package org.validoc.utils.monads

import scala.concurrent.{ExecutionContext, Future}

trait FlatMap[M[_]] {
  def flatMap[T, T2](m: M[T], fn: T => M[T2]): M[T2]
}

object FlatMap {
  implicit def FlatMapForFuture(implicit executionContext: ExecutionContext) = new FlatMap[Future] {
    override def flatMap[A, B](a: Future[A], fn: (A) => Future[B]): Future[B] = a.flatMap(fn)
  }
}

trait Monad[M[_]] extends FlatMap[M] {
  def lift[T](t: T): M[T]

  def map[T, T2](m: M[T], fn: T => T2): M[T2]

  def flatMap[T, T2](m: M[T], fn: T => M[T2]): M[T2]
}


