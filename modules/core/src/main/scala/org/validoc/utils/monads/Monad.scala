package org.validoc.utils.monads

import scala.util.Try
import scala.language.higherKinds

trait FlatMap[M[_]] {
  def flatMap[T, T2](m: M[T], fn: T => M[T2]): M[T2]

}

trait CanMap[M[_]] {
  def map[T, T2](m: M[T], fn: T => T2): M[T2]
}
object CanMap{
  implicit class ArrowPimper[M[_] : CanMap, P, R](fn: P => M[R]) {
    def >>>[R2](fn2: R => R2) = { p: P => implicitly[CanMap[M]].map[R, R2](fn(p), fn2) }
  }
}


trait Monad[M[_]] extends FlatMap[M] with CanMap[M] {
  def lift[T](t: => T): M[T]

  /** This may just throw the exception if it's not meaningfull to lift it. Meaningful monads include Try, Future ... */
  def liftTry[T](tryT: Try[T]): M[T]

  def join2[T1, T2](m1: M[T1], m2: M[T2]): M[(T1, T2)] = flatMap(m1, (t1: T1) => flatMap(m2, (t2: T2) => lift((t1, t2))))

  @unchecked
  def join[T](ms: Seq[M[T]]): M[Seq[T]] = ms.foldLeft(lift(Seq[T]())) {  (mAcc: M[Seq[T]], mV: M[T]) => flatMap(mAcc, (acc: Seq[T]) => flatMap(mV, (v: T) => lift(acc :+ v))) }

}


