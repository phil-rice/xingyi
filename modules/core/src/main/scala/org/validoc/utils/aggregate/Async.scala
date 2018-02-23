package org.validoc.utils.aggregate

import scala.collection.concurrent.TrieMap
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

trait Async[M[_]] {
  def respond[T](m: M[T], fn: Try[T] => Unit): M[T]
  def liftM[T](t: T): M[T]
  def exception[T](t: Throwable): M[T]
  def await[T](m: M[T]): T
}

trait WithFailure[M[_], Failure] {
  def onComplete[T](m: M[T], fn: Try[Either[Failure, T]] => Unit): M[T]
  def fail[T](f: Failure): M[T]
}

trait AsyncFunctor[M[_]] extends Async[M] {
  def map[T, T1](m: M[T], fn: T => T1): M[T1]
}

trait AsyncMonad[M[_]] extends AsyncFunctor[M] {
  def flatMap[T, T1](m: M[T], fn: T => M[T1]): M[T1]
  def flattenM[T](seq: Seq[M[T]]): M[Seq[T]]
}

trait FailureMaker[X, Failure] extends (X => Failure)

object AnyPimpers extends AnyPimpers

trait AnyPimpers {

  implicit class AnyPimper[T](t: T) {
    def liftM[M[_]](implicit async: Async[M]): M[T] = async.liftM(t)
    def |>[T1](fn: T => T1): T1 = fn(t)
    def |+>[T1](fn: T => T => T1): T1 = fn(t)(t)
    def liftException[M[_], T1](implicit async: Async[M], ev: T <:< Throwable): M[T1] = async.exception(t)
    def |?[M[_] : Async, Failure](validation: T => Seq[Failure])(implicit withFailure: WithFailure[M, Failure], multipleFailures: FailureMaker[Seq[Failure], Failure]): M[T] =
      validation(t) match {
        case Nil => t.liftM
        case Seq(single) => withFailure.fail(single)
        case f => withFailure.fail(multipleFailures(f))
      }

  }

  implicit class AsyncFailurePimper[Failure](f: Failure) {
    def fail[M[_], T](implicit async: WithFailure[M, Failure]): M[T] = async.fail[T](f)
  }

  implicit class SeqOfMonadPimper[M[_], T](seq: Seq[M[T]])(implicit async: AsyncMonad[M]) {
    def flattenM: M[Seq[T]] = async.flattenM(seq)
  }


  implicit class FunctionToSeqPimper[Req, Res](fn: Req => Seq[Res]) {
    def withReq[Res2](fn2: Res => Res2): (Req => Seq[(Res, Res2)]) = fn andThen (_.map(res => (res, fn2(res))))
  }


}

trait FailurePimpers {

  def liftTry[M[_], T](tryT: Try[T])(implicit async: Async[M]): M[T] = tryT match {
    case Success(t) => async.liftM(t)
    case Failure(t) => Async.liftException(t)
  }

  def liftException[M[_], T](throwable: Throwable)(implicit async: Async[M]): M[T] = async.exception(throwable)

  implicit class AsyncSeqFailurePimper[Failure](s: Seq[Failure]) {
    def singleFailure[M[_], T](implicit asyncWithFailure: WithFailure[M, Failure], multipleFailures: FailureMaker[Seq[Failure], Failure]): M[T] = s match {
      case Seq(single) => asyncWithFailure.fail(single)
      case other => asyncWithFailure.fail(multipleFailures(other))
    }
  }

}

trait AsyncPimpers {

  implicit class AsyncPimper[M[_], T](m: M[T])(implicit async: Async[M]) {
    def respond(fn: Try[T] => Unit): M[T] = async.respond(m, fn)
    def await: T = async.await(m)


    //with failure
    def onComplete[Failure, T1](fn: Try[Either[Failure, T]] => Unit)(implicit async: WithFailure[M, Failure]): M[T] = async.onComplete(m, fn)
  }

  implicit class AsyncFunctorPimper[M[_], T](m: M[T])(implicit async: AsyncFunctor[M]) {
    def map[T1](fn: T => T1): M[T1] = async.map(m, fn)
    def |>[T1](fn: T => T1): M[T1] = async.map(m, fn)
    def |+>[T1](fn: T => T => T1): M[T1] = async.map[T, T1](m, t => fn(t)(t))
  }

  implicit class AsyncMonadPimper[M[_], T](m: M[T])(implicit async: AsyncMonad[M]) {
    def flatMap[T1](fn: T => M[T1]): M[T1] = async.flatMap(m, fn)
    def |=>[T1](fn: T => M[T1]): M[T1] = async.flatMap(m, fn)
    def |==>[T1](fn: T => Seq[M[T1]]): M[Seq[T1]] = async.flatMap(m, { req: T => async.flattenM(fn(req)) })
    def |=+>[T1](fn: T => T => M[T1]): M[T1] = async.flatMap(m, { t: T => fn(t)(t) })
  }


}

trait AsyncWithFailurePimpers extends AnyPimpers with AsyncPimpers with FailurePimpers {

  implicit object FailureMakerForThrowable extends FailureMaker[Seq[Throwable], Throwable] {
    override def apply(v1: Seq[Throwable]): Throwable = v1.toList match {
      case Nil => throw new IllegalArgumentException("Should not call failure maker when have no failures")
      case Seq(one) => one
      case head :: tail => new MultipleExceptions(head, tail)
    }
  }

  implicit class AsyncMonadWithFailurePimper[M[_], T](m: M[T])(implicit async: AsyncMonad[M]) {
    def |=?[Failure](validation: T => Seq[Failure])(implicit withFailure: WithFailure[M, Failure], multipleFailures: FailureMaker[Seq[Failure], Failure]): M[T] =
      m.flatMap[T] { res => res |?[M, Failure] validation }
  }
}


object Async extends AnyPimpers with AsyncWithFailurePimpers with LocalOpsPimper

