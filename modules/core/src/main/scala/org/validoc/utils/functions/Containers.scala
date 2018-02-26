package org.validoc.utils.functions

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}


trait Functor[M[_]] {
  def map[T, T1](m: M[T], fn: T => T1): M[T1]
}

trait Monad[M[_]] extends Functor[M] {
  def liftM[T](t: T): M[T]
  def flatMap[T, T1](m: M[T], fn: T => M[T1]): M[T1]

  def join2[T1, T2](m1: M[T1], m2: M[T2]): M[(T1, T2)] = flatMap(m1, (t1: T1) => flatMap(m2, (t2: T2) => liftM((t1, t2))))
  def join3[T1, T2, T3](m1: M[T1], m2: M[T2], m3: M[T3]): M[(T1, T2, T3)] = flatMap(m1, (t1: T1) => flatMap(m2, (t2: T2) => flatMap(m3, (t3: T3) => liftM((t1, t2, t3)))))
  def join4[T1, T2, T3, T4](m1: M[T1], m2: M[T2], m3: M[T3], m4: M[T4]): M[(T1, T2, T3, T4)] = flatMap(m1, (t1: T1) => flatMap(m2, (t2: T2) => flatMap(m3, (t3: T3) => flatMap(m4, (t4:T4) => liftM((t1, t2, t3, t4))))))
  def join5[T1, T2, T3, T4, T5](m1: M[T1], m2: M[T2], m3: M[T3], m4: M[T4], m5: M[T5]): M[(T1, T2, T3, T4, T5)] = flatMap(m1, (t1: T1) => flatMap(m2, (t2: T2) => flatMap(m3, (t3: T3) => flatMap(m4, (t4:T4) => flatMap(m5, (t5:T5) => liftM((t1, t2, t3, t4, t5)))))))
  def flattenM[T](ms: Seq[M[T]]): M[Seq[T]] = ms.foldLeft(liftM(Seq[T]())) { (mAcc: M[Seq[T]], mV: M[T]) => flatMap(mAcc, (acc: Seq[T]) => flatMap(mV, (v: T) => liftM(acc :+ v))) }
}

trait MonadWithException[M[_]] extends Monad[M] {
  def exception[T](t: Throwable): M[T]
  def recover[T](m: M[T], fn: Exception => T): M[T]

  def liftTry[T](t: Try[T]): M[T] = t.fold(exception, liftM)
  def foldException[T, T1](m: M[T], fnE: Exception => T1, fn: T => T1): M[T1] = recover[T1](map(m, fn), fnE)
}

trait MonadCanFail[M[_], Fail] extends MonadWithException[M] {
  def fail[T](f: Fail): M[T]
  def foldWithFail[T, T1](m: M[T], fnE: Exception => M[T1], fnFailure: Fail => M[T1], fn: T => M[T1]): M[T1]
  def onComplete[T](m: M[T], fn: Try[Either[Fail, T]] => Unit): M[T] = foldWithFail[T, T](m,
    { e: Exception => fn(Failure(e)); m },
    { f: Fail => fn(Success(Left(f))); m },
    { t: T => fn(Success(Right(t))); m }
  )
}

trait FailureMaker[X, Fail] extends (X => Fail)
