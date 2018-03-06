package org.validoc.utils.language

import org.validoc.utils.functions._

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

trait FunctorLanguage {

  implicit class FunctorPimper[M[_], T](m: M[T])(implicit functor: Functor[M]) {
    def map[T1](fn: T => T1): M[T1] = functor.map(m, fn)
    def |=>[T2](fn: T => T2): M[T2] = functor.map(m, fn)
    def |+>[T1](fn: T => T => T1): M[T1] = functor.map[T, T1](m, t => fn(t)(t))
  }

}

trait MonadLanguage extends FunctorLanguage {

  def join2WithReq[M[_], Req, Res1, Res2](firstService: Req => M[Res1], secondService: Req => M[Res2])(implicit monad: Monad[M]): Req => M[(Req, Res1, Res2)] = { req: Req => monad.join3(monad.liftM(req), firstService(req), secondService(req)) }
  def join3WithReq[M[_], Req, Res1, Res2, Res3](firstService: Req => M[Res1], secondService: Req => M[Res2], thirdService: Req => M[Res3])(implicit monad: Monad[M]): Req => M[(Req, Res1, Res2, Res3)] = { req: Req => monad.join4(monad.liftM(req), firstService(req), secondService(req), thirdService(req)) }
  def join4WithReq[M[_], Req, Res1, Res2, Res3, Res4](firstService: Req => M[Res1], secondService: Req => M[Res2], thirdService: Req => M[Res3], fouthService: Req => M[Res4])(implicit monad: Monad[M]): Req => M[(Req, Res1, Res2, Res3, Res4)] = { req: Req => monad.join5(monad.liftM(req), firstService(req), secondService(req), thirdService(req), fouthService(req)) }

  implicit class MonadPimper[M[_], T](m: M[T])(implicit monad: Monad[M]) {
    def flatMap[T1](fn: T => M[T1]): M[T1] = monad.flatMap(m, fn)
    def |==>[T2](fn: T => M[T2]): M[T2] = monad.flatMap(m, fn)
    def |=*>[T1](fn: T => Seq[M[T1]]): M[Seq[T1]] = monad.flatMap(m, { req: T => monad.flattenM(fn(req)) })
    def |=+>[T1](fn: T => T => M[T1]): M[T1] = monad.flatMap(m, { t: T => fn(t)(t) })
  }

  implicit class MonadWithExceptionPimper[M[_], T](m: M[T])(implicit monad: MonadWithException[M]) {
    def foldException[T1](fnE: Throwable => M[T1], fn: T => M[T1]): M[T1] = monad.foldException(m, fnE, fn)
    def mapTry[T1](fn: Try[T] => M[T1]): M[T1] = monad.foldException(m, t => fn(Failure(t)), { t: T => fn(Success(t)) })
    def registerSideeffect(fn: Try[T] => Unit): M[T] = monad.foldException(m, { e => fn(Failure(e)); monad.exception(e) }, { t: T => fn(Success(t)); monad.liftM(t) })
  }


  implicit class MonadCanFailWithExceptionPimper[M[_], T](m: M[T]) {
    def foldWithExceptionAndFail[Fail, T1](fnE: Throwable => M[T1], fnFailure: Fail => M[T1], fn: T => M[T1])(implicit async: MonadCanFailWithException[M, Fail]): M[T1] =
      async.foldWithExceptionAndFail[T, T1](m, fnE, fnFailure, fn)
    def onComplete[Fail](fn: Try[Either[Fail, T]] => Unit)(implicit monad: MonadCanFailWithException[M, Fail]): M[T] = monad.onComplete(m, fn)

  }


  implicit class SeqOfMonadPimper[M[_], T](seq: Seq[M[T]])(implicit async: Monad[M]) {
    def flattenM: M[Seq[T]] = async.flattenM(seq)
  }


  implicit class MonadSeqPimper[M[_], T](seq: M[Seq[T]])(implicit async: Monad[M]) {
    def |>[T2](fn: T => T2): M[Seq[T2]] = async.map[Seq[T], Seq[T2]](seq, _.map(fn))
    def |+>[T2](fn: T => M[T2]): M[Seq[T2]] = async.flatMap[Seq[T], Seq[T2]](seq, x => async.flattenM(x.map(fn)))
  }

  implicit class Tuple2MonadPimper[M[_], T1, T2](tuple: (M[T1], M[T2]))(implicit async: Monad[M]) {
    def join[T](fn: (T1, T2) => T): M[T] = async.map(async.join2(tuple._1, tuple._2), fn.tupled)

  }

  implicit class TupleFn2MonadPimper[M[_], Req, Res1, Res2](tuple: (Req => M[Res1], Req => M[Res2]))(implicit async: Monad[M]) {
    def join[T](fn: (Res1, Res2) => T): Req => M[T] = { req => async.map(async.join2(tuple._1(req), tuple._2(req)), fn.tupled) }
  }

  implicit class MonadCanFailPimper[M[_], T](m: M[T]) {

    def mapEither[Fail, T1](fn: Either[Fail, T] => M[T1])(implicit monadCanFail: MonadCanFail[M, Fail]): M[T1] = monadCanFail.mapEither(m, fn)
    def |=?[Failure](validation: T => Seq[Failure])(implicit monad: MonadCanFail[M, Failure], monoid: Monoid[Failure]): M[T] = {
      monad.flatMap[T, T](m, { res =>
        validation(res) match {
          case Nil => monad.liftM(res)
          case s => monad.fail(monoid.addAll(s))
        }

      })
    }
  }

}







