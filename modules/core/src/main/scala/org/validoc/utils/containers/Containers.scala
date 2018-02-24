package org.validoc.utils.containers

import scala.util.{Failure, Success, Try}
import scala.language.higherKinds


trait Functor[M[_]] {
  def map[T, T1](m: M[T], fn: T => T1): M[T1]
}

trait Monad[M[_]] extends Functor[M] {
  def liftM[T](t: T): M[T]
  def flatMap[T, T1](m: M[T], fn: T => M[T1]): M[T1]

  def join2[T1, T2](m1: M[T1], m2: M[T2]): M[(T1, T2)] = flatMap(m1, (t1: T1) => flatMap(m2, (t2: T2) => liftM((t1, t2))))
  def flattenM[T](ms: Seq[M[T]]): M[Seq[T]] = ms.foldLeft(liftM(Seq[T]())) { (mAcc: M[Seq[T]], mV: M[T]) => flatMap(mAcc, (acc: Seq[T]) => flatMap(mV, (v: T) => liftM(acc :+ v))) }
}

trait MonadWithException [M[_]]extends Monad[M]{
  def exception[T](t: Throwable): M[T]
  def liftTry[T](t: Try[T]): M[T] = t.fold(exception, liftM)
  def foldException[T, T1](m: M[T], fnE: Exception => T1, fn: T => T1): M[T1] = recover[T1](map(m, fn), fnE)
  def recover[T](m: M[T], fn: Exception => T): M[T]

}
trait MonadCanFail[M[_], Fail] extends MonadWithException[M] {
  def fold[T, T1](m: M[T], fnE: Exception => M[T1], fnFailure: Fail => M[T1], fn: T => M[T1]): M[T1]
  def onComplete[T](m: M[T], fn: Try[Either[Fail, T]] => Unit): M[T] = fold[T, T](m,
    { e: Exception => fn(Failure(e)); m },
    { f: Fail => fn(Success(Left(f))); m },
    { t: T => fn(Success(Right(t))); m }
  )
  def fail[T](f: Fail): M[T]
}


trait FailureMaker[X, Fail] extends (X => Fail)
