package one.xingyi.core.language

import one.xingyi.core.functions._

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

    def |=*>[T1](fn: T => Seq[M[T1]]): M[Seq[T1]] = monad.flatMap(m, { req: T => monad.flattenM(fn(req)) })
    def |=+>[T1](fn: T => T => M[T1]): M[T1] = monad.flatMap(m, { t: T => fn(t)(t) })
  }

  implicit class MonadWithExceptionPimper[M[_], T](m: M[T])(implicit monad: MonadWithException[M]) {
    def registerSideeffect(fn: Try[T] => Unit): M[T] = monad.foldException(m, { e => fn(Failure(e)); monad.exception(e) }, { t: T => fn(Success(t)); monad.liftM(t) })
  }


  implicit class MonadCanFailWithExceptionPimper[M[_], T](m: M[T]) {
    def onComplete[Fail](fn: Try[Either[Fail, T]] => Unit)(implicit monad: MonadCanFailWithException[M, Fail]): M[T] = monad.onComplete(m, fn)

  }

}







