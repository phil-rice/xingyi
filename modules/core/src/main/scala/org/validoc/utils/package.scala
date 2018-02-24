package org.validoc

import org.validoc.utils.concurrency.{Async, MultipleExceptions}
import org.validoc.utils.containers._

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

package object utils {

  type Service[M[_], Req, Res] = (Req => M[Res])
  type Parser[T] = String => T

  implicit class AnyPimper[T](t: T) {
    def |>[T2](fn: T => T2) = fn(t)
    def liftM[M[_]](implicit monad: Monad[M]): M[T] = monad.liftM(t)
    def |+>[T1](fn: T => T => T1): T1 = fn(t)(t)
    def liftException[M[_], T1](implicit async: MonadWithException[M], ev: T <:< Throwable): M[T1] = async.exception(t)
    def |?[M[_] : Functor, Failure](validation: T => Seq[Failure])(implicit withFailure: MonadCanFail[M, Failure], multipleFailures: FailureMaker[Seq[Failure], Failure]): M[T] =
      validation(t) match {
        case Nil => t.liftM
        case Seq(single) => withFailure.fail(single)
        case f => withFailure.fail(multipleFailures(f))
      }
  }

  implicit class TryPimper[T](tryT: Try[T]) {
    def liftTry[M[_]](implicit monadWithException: MonadWithException[M])= monadWithException.liftTry(tryT)
  }

  implicit class FunctionPimper[Req, Res](fn: Req => Res) {
    def ~>[Res2](fn2: Res => Res2): (Req) => Res2 = { res: Req => fn2(fn(res)) }
  }


  implicit class FunctorPimper[M[_], T](m: M[T])(implicit functor: Functor[M]) {
    def map[T1](fn: T => T1): M[T1] = functor.map(m, fn)
    def |=>[T2](fn: T => T2): M[T2] = functor.map(m, fn)
    def |+>[T1](fn: T => T => T1): M[T1] = functor.map[T, T1](m, t => fn(t)(t))
  }

  implicit class MonadPimper[M[_], T](m: M[T])(implicit monad: Monad[M]) {
    def flatMap[T1](fn: T => M[T1]): M[T1] = monad.flatMap(m, fn)
    def |==>[T2](fn: T => M[T2]): M[T2] = monad.flatMap(m, fn)
    def |=*>[T1](fn: T => Seq[M[T1]]): M[Seq[T1]] = monad.flatMap(m, { req: T => monad.flattenM(fn(req)) })
    def |=+>[T1](fn: T => T => M[T1]): M[T1] = monad.flatMap(m, { t: T => fn(t)(t) })
  }

  implicit class MonadWithExceptionPimper[M[_], T](m: M[T])(implicit monad: MonadWithException[M]) {
    def foldException[T1](fnE: Exception => T1, fn: T => T1): M[T1] = monad.foldException(m, fnE, fn)
    def mapTry[T1](fn: Try[T] => T1): M[T1] = monad.foldException(m, t => fn(Failure(t)), { t: T => fn(Success(t)) })
    def registerSideeffect(fn: Try[T] => Unit): M[T] = monad.foldException(m, { e: Exception => fn(Failure(e)); throw e }, { t: T => fn(Success(t)); t })
  }

  implicit class MonadCanFailPimper[M[_], T](m: M[T]) {
    def fold[Fail, T1](fnE: Exception => M[T1], fnFailure: Fail => M[T1], fn: T => M[T1])(implicit async: MonadCanFail[M, Fail]): M[T1] = async.fold[T, T1](m, fnE, fnFailure, fn)
    //    def foldTry[Fail, T1](fnTry: Try[T] => T1)(implicit async: MonadCanFail[M, Fail]) = monad.fold(m, t => fnTry(Failure(t)), { t: T => fnTry(Success(t)) })
    def onComplete[Fail](fn: Try[Either[Fail, T]] => Unit)(implicit async: MonadCanFail[M, Fail]): M[T] = async.onComplete(m, fn)

    //    def transformAndLift[Fail, Res](fnThrowable: Throwable => Res, fnMap: T => Res)(implicit async: MonadCanFail[M, Fail]) =
    //      async.transformWithFail[T, Res](m, e => fnThrowable(e).liftM[M], fail ) { case Success(t) => fnMap(t); case Failure(t) => fnThrowable(t) }
    def |=?[Failure](validation: T => Seq[Failure])(implicit withFailure: MonadCanFail[M, Failure], multipleFailures: FailureMaker[Seq[Failure], Failure]): M[T] =
      m.flatMap[T] { res => res |?[M, Failure] validation }
  }


  implicit class MonadSeqPimper[M[_], T](seq: M[Seq[T]])(implicit async: Monad[M]) {
    def |>[T2](fn: T => T2) = async.map[Seq[T], Seq[T2]](seq, _.map(fn))
    def |+>[T2](fn: T => M[T2]) = async.flatMap[Seq[T], Seq[T2]](seq, x => async.flattenM(x.map(fn)))
  }


  implicit class MonadFunctionPimper[M[_], Req, Res](fn: Req => M[Res])(implicit monad: Monad[M]) {
    def |=>[Res2](mapFn: Res => Res2): (Req => M[Res2]) = req => monad.map(fn(req), mapFn)
    def |+>[Res2](mapFn: (Req => Res => Res2)): (Req => M[Res2]) = req => monad.map(fn(req), mapFn(req))
    def |==>[Res2](mapFn: Res => M[Res2]): (Req => M[Res2]) = req => monad.flatMap(fn(req), mapFn)

    def split[Res1, Res2](mapFn1: Res => M[Res1], mapFn2: Res => M[Res2]): (Req => M[(Res1, Res2)]) = { req =>
      val res = fn(req)
      val res1 = monad.flatMap(res, mapFn1)
      val res2 = monad.flatMap(res, mapFn2)
      monad.join2(res1, res2)
    }
    def tee[Res1](mapFn2: Res => M[Res1]): (Req => M[(Res, Res1)]) = { req =>
      val res = fn(req)
      val res2 = monad.flatMap(res, mapFn2)
      monad.join2(res, res2)
    }
  }

  implicit class MonadWithExceptionFunctionPimper[M[_], Req, Res](fn: Req => M[Res])(implicit monad: MonadWithException[M]) {
    def foldException[Res2](fnThrowable: Throwable => Res2, fnMap: Res => Res2): (Req => M[Res2]) = { req => monad.foldException[Res, Res2](fn(req), fnThrowable, fnMap) }
  }

  implicit class MonadSeqFunctionPimper[M[_], Req, Res](fn: Req => M[Seq[Res]])(implicit async: Monad[M]) {
    def |*=>[Res2](mapFn: Res => Res2): (Req => M[Seq[Res2]]) = { req => async.map[Seq[Res], Seq[Res2]](fn(req), _.map(mapFn)) }
    def |*==>[Res2](mapFn: Res => M[Res2]): (Req => M[Seq[Res2]]) = { req =>
      async.flatMap[Seq[Res], Seq[Res2]](fn(req), { seqRes => async.flattenM(seqRes.map(mapFn))
      })
    }
  }


  implicit class SeqFunctionPimper[Req, Res](fn: Req => Seq[Res]) {
    def ~>[Res2](fn2: Res => Res2): (Req) => Seq[Res2] = { res: Req => fn(res).map(fn2) }

    def ~~>[M[_], Res2](fn2: Res => M[Res2])(implicit async: Monad[M]): Req => M[Seq[Res2]] = { res: Req => async.flattenM(fn(res).map(fn2)) }
  }

  implicit class Tuple2MonadPimper[M[_], T1, T2](tuple: (M[T1], M[T2]))(implicit async: Monad[M]) {
    def join[T](fn: (T1, T2) => T): M[T] = async.map(async.join2(tuple._1, tuple._2), fn.tupled)

  }

  implicit class TupleFn2MonadPimper[M[_], Req, Res1, Res2](tuple: (Req => M[Res1], Req => M[Res2]))(implicit async: Monad[M]) {
    def join[T](fn: (Res1, Res2) => T): Req => M[T] = { req => async.map(async.join2(tuple._1(req), tuple._2(req)), fn.tupled) }
  }


  implicit class AsyncFailurePimper[Failure](f: Failure) {
    def fail[M[_], T](implicit async: MonadCanFail[M, Failure]): M[T] = async.fail[T](f)
  }

  implicit class SeqOfMonadPimper[M[_], T](seq: Seq[M[T]])(implicit async: Monad[M]) {
    def flattenM: M[Seq[T]] = async.flattenM(seq)
  }


  implicit class FunctionToSeqPimper[Req, Res](fn: Req => Seq[Res]) {
    def withReq[Res2](fn2: Res => Res2): (Req => Seq[(Res, Res2)]) = fn andThen (_.map(res => (res, fn2(res))))
  }


  def liftTry[M[_], T](tryT: Try[T])(implicit monad: MonadWithException[M]): M[T] = tryT match {
    case Success(t) => monad.liftM(t)
    case Failure(t) => monad.exception(t)
  }

  implicit class AsyncSeqFailurePimper[Failure](s: Seq[Failure]) {
    def singleFailure[M[_], T](implicit asyncWithFailure: MonadCanFail[M, Failure], multipleFailures: FailureMaker[Seq[Failure], Failure]): M[T] = s match {
      case Seq(single) => asyncWithFailure.fail(single)
      case other => asyncWithFailure.fail(multipleFailures(other))
    }
  }


  implicit class AsyncPimper[M[_], T](m: M[T])(implicit async: Async[M]) {
    def respond(fn: Try[T] => Unit): M[T] = async.respond(m, fn)
    def await(): T = async.await(m)
  }


  implicit object FailureMakerForThrowable extends FailureMaker[Seq[Throwable], Throwable] {
    override def apply(v1: Seq[Throwable]): Throwable = v1.toList match {
      case Nil => throw new IllegalArgumentException("Should not call failure maker when have no failures")
      case Seq(one) => one
      case head :: tail => new MultipleExceptions(head, tail)
    }
  }


}