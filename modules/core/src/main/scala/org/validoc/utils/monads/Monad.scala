package org.validoc.utils.monads

import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds
import scala.util.Try

trait FlatMap[M[_]] {
  def flatMap[T, T2](m: M[T], fn: T => M[T2]): M[T2]

}

trait CanMap[M[_]] {
  def map[T, T2](m: M[T], fn: T => T2): M[T2]
}

object CanMap {

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
  def join[T](ms: Seq[M[T]]): M[Seq[T]] = ms.foldLeft(lift(Seq[T]())) { (mAcc: M[Seq[T]], mV: M[T]) => flatMap(mAcc, (acc: Seq[T]) => flatMap(mV, (v: T) => lift(acc :+ v))) }

}

object Monad {

  implicit def MonadForFuture(implicit ec: ExecutionContext) = new Monad[Future] {
    override def lift[T](t: => T): Future[T] = Future.successful(t)

    /** This may just throw the exception if it's not meaningfull to lift it. Meaningful monads include Try, Future ... */
    override def liftTry[T](tryT: Try[T]): Future[T] = Future.fromTry(tryT)

    override def map[T, T2](m: Future[T], fn: (T) => T2): Future[T2] = m.map(fn)

    override def flatMap[T, T2](m: Future[T], fn: (T) => Future[T2]): Future[T2] = m.flatMap(fn)
  }

  implicit object MonadForOption extends Monad[Option] {
    override def lift[T](t: => T): Option[T] = Some(t)

    /** This may just throw the exception if it's not meaningfull to lift it. Meaningful monads include Try, Future ... */
    override def liftTry[T](tryT: Try[T]): Option[T] = tryT.toOption

    override def map[T, T2](m: Option[T], fn: (T) => T2): Option[T2] = m.map(fn)

    override def flatMap[T, T2](m: Option[T], fn: (T) => Option[T2]): Option[T2] = m.flatMap(fn)
  }

  implicit object MonadForList extends Monad[List] {
    override def lift[T](t: => T): List[T] = List(t)

    /** This may just throw the exception if it's not meaningfull to lift it. Meaningful monads include Try, Future ... */
    override def liftTry[T](tryT: Try[T]): List[T] = tryT.toOption.toList

    override def flatMap[T, T2](m: List[T], fn: (T) => List[T2]): List[T2] =m.flatMap(fn)

    override def map[T, T2](m: List[T], fn: (T) => T2): List[T2] = m.map(fn)
  }

}


trait MonadLibrary {

  implicit class MonadPimper[M[_], T](m: M[T])(implicit monad: Monad[M]) {
    def map[T1](fn: T => T1): M[T1] = monad.map(m, fn)

    def flatMap[T1](fn: T => M[T1]): M[T1] = monad.flatMap(m, fn)
  }

}

object MonadLibrary extends MonadLibrary