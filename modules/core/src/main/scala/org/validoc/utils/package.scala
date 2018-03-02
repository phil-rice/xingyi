package org.validoc

import org.validoc.utils.functions._
import org.validoc.utils.success.{ExceptionState, FailedState, SucceededState, SuccessState}

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

package object utils {

  type Service[M[_], Req, Res] = (Req => M[Res])

  def withValue[X, Y](x: X)(fn: X => Y) = fn(x)
  def sideeffect[X, Y](x: X)(fn: X => Y) = {
    fn(x);
    x
  }

  def join2WithReq[M[_], Req, Res1, Res2](firstService: Req => M[Res1], secondService: Req => M[Res2])(implicit monad: Monad[M]) = { req: Req => monad.join3(req.liftM, firstService(req), secondService(req)) }
  def join3WithReq[M[_], Req, Res1, Res2, Res3](firstService: Req => M[Res1], secondService: Req => M[Res2], thirdService: Req => M[Res3])(implicit monad: Monad[M]) = { req: Req => monad.join4(req.liftM, firstService(req), secondService(req), thirdService(req)) }
  def join4WithReq[M[_], Req, Res1, Res2, Res3, Res4](firstService: Req => M[Res1], secondService: Req => M[Res2], thirdService: Req => M[Res3], fouthService: Req => M[Res4])(implicit monad: Monad[M]) = { req: Req => monad.join5(req.liftM, firstService(req), secondService(req), thirdService(req), fouthService(req)) }

  implicit class AnyPimper[T](t: T) {
    def |>[T2](fn: T => T2) = fn(t)
    def liftM[M[_]](implicit container: Liftable[M]): M[T] = container.liftM(t)
    def |+>[T1](fn: T => T => T1): T1 = fn(t)(t)
    def liftException[M[_], T1](implicit async: MonadWithException[M], ev: T <:< Throwable): M[T1] = async.exception(t)
    def |?[M[_] : Functor, Failure](validation: T => Seq[Failure])(implicit withFailure: MonadCanFail[M, Failure], monoid: Monoid[Failure]): M[T] =
      validation(t) match {
        case Nil => t.liftM
        case s => withFailure.fail(monoid.addAll(s))
      }
  }

  implicit class BooleanPimper(boolean: Boolean) {
    def toOption[T](value: => T) = if (boolean) Some(value) else None
  }

  implicit class EitherPimper[L, R](either: Either[L, R]) {
    def liftEither[M[_]](implicit monad: MonadCanFail[M, L]): M[R] = either.fold(_.fail, _.liftM)
    def getOrException(exceptionCreator: L => Throwable): R =
      either match {
        case Right(r) => r
        case Left(l) => throw exceptionCreator(l)
      }
  }

  implicit class TryPimper[T](tryT: Try[T]) {
    def liftTry[M[_]](implicit monadWithException: MonadWithException[M]) = monadWithException.liftTry(tryT)
  }

  implicit class FunctionPimper[Req, Res](fn: Req => Res) {
    def ~>[Res2](fn2: Res => Res2): (Req) => Res2 = { res: Req => fn2(fn(res)) }
    def ~^>(fn2: Res => Unit): (Req => Res) = { req: Req => sideeffect(fn(req))(fn2) }
    def ~+>[Res2](fn2: Req => Res => Res2): (Req => Res2) = { req: Req => fn2(req)(fn(req)) }
    //    def let[Mid, Res2](mid: Req => Mid)(fn2: Mid => Res => Res): Req => Res = { req: Req => fn2(mid(req))(fn(req)) }
    def onEnterAndExit[Mid](mid: Req => Mid, before: Mid => Unit, after: (Mid, Res) => Unit) = { req: Req =>
      val m = mid(req)
      before(m)
      val result = fn(req)
      after(m, result)
      result
    }
  }


  implicit class OptionFunctionCurriedPimper[T, T1, T2](fn: T => T1 => Option[T2]) {
    def chain(fn2: T => T1 => Option[T2]): T => T1 => Option[T2] = { t: T =>
      t1: T1 =>
        fn(t)(t1) match {
          case None => fn2(t)(t1)
          case x => x
        }
    }
  }

  implicit class Function2Pimper[Req1, Req2, Res](fn: (Req1, Req2) => Res) {
    def ~>[Res2](fn2: Res => Res2): (Req1, Req2) => Res2 = { (r1, r2) => fn2(fn(r1, r2)) }
  }

  implicit class SeqFunctionPimper[Req, Res](fn: Req => Seq[Res]) {
    def ~>[Res2](fn2: Res => Res2): (Req) => Seq[Res2] = { res: Req => fn(res).map(fn2) }

    def ~~>[M[_], Res2](fn2: Res => M[Res2])(implicit async: Monad[M]): Req => M[Seq[Res2]] = { req: Req => async.flattenM(fn(req).map(fn2)) }
    def ~+>[M[_], Res2](fn2: Res => M[Res2])(implicit async: Monad[M]): Req => M[Seq[(Res, Res2)]] = { req: Req =>
      async.flattenM(fn(req).map(r => fn2(r).map(res2 => (r, res2))))

    }
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
    def foldException[T1](fnE: Exception => M[T1], fn: T => M[T1]): M[T1] = monad.foldException(m, fnE, fn)
    def mapTry[T1](fn: Try[T] => M[T1]): M[T1] = monad.foldException(m, t => fn(Failure(t)), { t: T => fn(Success(t)) })
    def registerSideeffect(fn: Try[T] => Unit): M[T] = monad.foldException(m, { e: Exception => fn(Failure(e)); monad.exception(e) }, { t: T => fn(Success(t)); monad.liftM(t) })
  }

  implicit class MonadCanFailPimper[M[_], T](m: M[T]) {
    def mapEither[Fail, T1](fn: Either[Fail, T] => M[T1])(implicit monadCanFail: MonadCanFail[M, Fail]): M[T1] = monadCanFail.mapEither(m, fn)
    def |=?[Failure](validation: T => Seq[Failure])(implicit withFailure: MonadCanFail[M, Failure], monoid: Monoid[Failure]): M[T] =
      m.flatMap[T] { res => res |?[M, Failure] validation }
  }

  implicit class MonadCanFailWithExceptionPimper[M[_], T](m: M[T]) {
    def foldWithExceptionAndFail[Fail, T1](fnE: Throwable => M[T1], fnFailure: Fail => M[T1], fn: T => M[T1])(implicit async: MonadCanFailWithException[M, Fail]): M[T1] =
      async.foldWithExceptionAndFail[T, T1](m, fnE, fnFailure, fn)
    def onComplete[Fail](fn: Try[Either[Fail, T]] => Unit)(implicit async: MonadCanFailWithException[M, Fail]): M[T] = async.onComplete(m, fn)

  }


  implicit class MonadSeqPimper[M[_], T](seq: M[Seq[T]])(implicit async: Monad[M]) {
    def |>[T2](fn: T => T2) = async.map[Seq[T], Seq[T2]](seq, _.map(fn))
    def |+>[T2](fn: T => M[T2]) = async.flatMap[Seq[T], Seq[T2]](seq, x => async.flattenM(x.map(fn)))
  }


  implicit class MonadFunctionPimper[M[_], Req, Res](fn: Req => M[Res])(implicit monad: Monad[M]) {

    def |=>[Res2](mapFn: Res => Res2): (Req => M[Res2]) = req => monad.map(fn(req), mapFn)
    def |=+>[Res2](mapFn: (Req => Res => Res2)): (Req => M[Res2]) = req => monad.map(fn(req), mapFn(req))
    def |=++>[Res2](mapFn: (Req => Res => Res => M[Res2])): (Req => M[Res2]) = { req => monad.flatMap(fn(req), { res: Res => mapFn(req)(res)(res) }) }
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


  implicit class MonadCanFailFunctionPimper[M[_], Req, Res](fn: Req => M[Res]) {
    def |=|>[Fail, Res2](mapFn: Res => Either[Fail, Res2])(implicit monad: MonadCanFailWithException[M, Fail]) = { req: Req =>
      fn(req).flatMap(res => mapFn(res).fold[M[Res2]]({ fail: Fail => monad.fail(fail) }, { res2: Res2 => monad.liftM(res2) }))
    }
    def enterAndExit[Fail, Mid](midFn: Req => Mid, sideeffectFn: (Mid, SucceededState[Fail, Res]) => Unit)(implicit monad: MonadCanFailWithException[M, Fail]): Req => M[Res] = { req: Req =>
      val mid = midFn(req)
      val m = fn(req)
      monad.foldWithExceptionAndFail[Res, Res](m, { t => sideeffectFn(mid, ExceptionState(t)); m }, { f => sideeffectFn(mid, FailedState(f)); m }, { res => sideeffectFn(mid, SuccessState(res)); m })

    }
    def sideeffect[Fail](sideeffectFn: Req => SucceededState[Fail, Res] => Unit)(implicit monad: MonadCanFailWithException[M, Fail]): Req => M[Res] = { req: Req =>
      val m = fn(req)
      monad.foldWithExceptionAndFail[Res, Res](m, { t => sideeffectFn(req)(ExceptionState(t)); m }, { f => sideeffectFn(req)(FailedState(f)); m }, { res => sideeffectFn(req)(SuccessState(res)); m })
    }
  }

  implicit class MonadWithExceptionFunctionPimper[M[_], Req, Res](fn: Req => M[Res])(implicit monad: MonadWithException[M]) {
    def foldException[Res2](fnThrowable: Throwable => M[Res2], fnMap: Res => M[Res2]): (Req => M[Res2]) = { req => monad.foldException[Res, Res2](fn(req), fnThrowable, fnMap) }
    def onEnterAndExitM[Mid](mid: Req => Mid, after: Mid => Try[Res] => Unit) = { req: Req =>
      withValue(mid(req))(m => fn(req).registerSideeffect(after(m)))
    }
  }

  implicit class MonadSeqFunctionPimper[M[_], Req, Res](fn: Req => M[Seq[Res]])(implicit async: Monad[M]) {
    def |*=>[Res2](mapFn: Res => Res2): (Req => M[Seq[Res2]]) = { req => async.map[Seq[Res], Seq[Res2]](fn(req), _.map(mapFn)) }
    def |*==>[Res2](mapFn: Res => M[Res2]): (Req => M[Seq[Res2]]) = { req =>
      async.flatMap[Seq[Res], Seq[Res2]](fn(req), { seqRes => async.flattenM(seqRes.map(mapFn))
      })
    }
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


  implicit class AsyncPimper[M[_], T](m: M[T])(implicit async: Async[M]) {
    def respond(fn: Try[T] => Unit): M[T] = async.respond(m, fn)
    def await(): T = async.await(m)
  }


}