package one.xingyi.core.functions

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}


trait Liftable[M[_]]{
  def liftM[T](t: T): M[T]

}
trait Functor[M[_]] extends Liftable[M]{
  def map[T, T1](m: M[T], fn: T => T1): M[T1]
}


trait Monad[M[_]] extends Functor[M] {
  def flatMap[T, T1](m: M[T], fn: T => M[T1]): M[T1]

  def join2[T1, T2](m1: M[T1], m2: M[T2]): M[(T1, T2)] = flatMap(m1, (t1: T1) => flatMap(m2, (t2: T2) => liftM((t1, t2))))
  def join3[T1, T2, T3](m1: M[T1], m2: M[T2], m3: M[T3]): M[(T1, T2, T3)] = flatMap(m1, (t1: T1) => flatMap(m2, (t2: T2) => flatMap(m3, (t3: T3) => liftM((t1, t2, t3)))))
  def join4[T1, T2, T3, T4](m1: M[T1], m2: M[T2], m3: M[T3], m4: M[T4]): M[(T1, T2, T3, T4)] = flatMap(m1, (t1: T1) => flatMap(m2, (t2: T2) => flatMap(m3, (t3: T3) => flatMap(m4, (t4: T4) => liftM((t1, t2, t3, t4))))))
  def join5[T1, T2, T3, T4, T5](m1: M[T1], m2: M[T2], m3: M[T3], m4: M[T4], m5: M[T5]): M[(T1, T2, T3, T4, T5)] = flatMap(m1, (t1: T1) => flatMap(m2, (t2: T2) => flatMap(m3, (t3: T3) => flatMap(m4, (t4: T4) => flatMap(m5, (t5: T5) => liftM((t1, t2, t3, t4, t5)))))))
  def flattenM[T](ms: Seq[M[T]]): M[Seq[T]] = ms.foldLeft(liftM(Seq[T]())) { (mAcc: M[Seq[T]], mV: M[T]) => flatMap(mAcc, (acc: Seq[T]) => flatMap(mV, (v: T) => liftM(acc :+ v))) }
}

trait MonadWithException[M[_]] extends Monad[M] {

  def exception[T](t: Throwable): M[T]
  def recover[T](m: M[T], fn: Throwable => M[T]): M[T]

  def liftTry[T](t: Try[T]): M[T] = t.fold(exception, liftM)
  def foldException[T, T1](m: M[T], fnE: Throwable => M[T1], fn: T => M[T1]): M[T1] = recover[T1](flatMap(m, fn), fnE)
}

//
//trait CompletableMonad[M[_],H[_]] extends MonadWithException[M]{
//  def makePromise[T]: H[T]
//  def monad[T](h: H[T]): M[T];
//  def complete[T](h: H[T], t: Try[T])
//
//}


trait LiftFailure[M[_], Fail] {
  def fail[T](f: Fail): M[T]
}

trait MonadCanFail[M[_], Fail] extends Monad[M] with LiftFailure[M, Fail] {
  def mapEither[T, T1](m: M[T], fn: Either[Fail, T] => M[T1]): M[T1]
}

class MonadCanFailForEither[Fail] extends MonadCanFail[({type λ[α] = Either[Fail, α]})#λ, Fail] {
  type M[T] = Either[Fail, T]
  override def fail[T](f: Fail): Either[Fail, T] = Left(f)
  override def liftM[T](t: T): Either[Fail, T] = Right(t)
  override def flatMap[T, T1](m: Either[Fail, T], fn: T => M[T1]): M[T1] = m.right.flatMap(fn)
  override def map[T, T1](m: Either[Fail, T], fn: T => T1): M[T1] = m.right.map(fn)
  override def mapEither[T, T1](m: Either[Fail, T], fn: Either[Fail, T] => M[T1]): Either[Fail, T1] = fn(m)
}

object MonadCanFail {
  implicit def monadCanFailForEither[Fail]: MonadCanFail[({type λ[α] = Either[Fail, α]})#λ, Fail] = new MonadCanFailForEither[Fail]
}

trait MonadCanFailWithException[M[_], Fail] extends MonadWithException[M] with MonadCanFail[M, Fail] {
  def foldWithExceptionAndFail[T, T1](m: M[T], fnE: Throwable => M[T1], fnFailure: Fail => M[T1], fn: T => M[T1]): M[T1]
  def onComplete[T](m: M[T], fn: Try[Either[Fail, T]] => Unit): M[T] = foldWithExceptionAndFail[T, T](m,
    { e: Throwable => fn(Failure(e)); exception(e) },
    { f: Fail => fn(Success(Left(f))); fail(f) },
    { t: T => fn(Success(Right(t))); liftM(t) }
  )
  def mapTryFail[T, T1](m: M[T], fn: Try[Either[Fail, T]] => M[T1]): M[T1] = foldWithExceptionAndFail[T, T1](m,
    t => fn(Failure(t)),
    f => fn(Success(Left(f))),
    t => fn(Success(Right(t))))

}

