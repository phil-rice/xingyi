package one.xingyi.utils.language

import one.xingyi.utils.exceptions.Exceptions
import one.xingyi.utils.functions.{Monad, MonadCanFail, MonadCanFailWithException, MonadWithException}
import one.xingyi.utils.language.Language.withValue

import scala.util.Try
import scala.language._

trait MonadFunctionLanguage extends MonadLanguage {

  implicit class MonadFunctionPimper[M[_], Req, Res](fn: Req => M[Res])(implicit monad: Monad[M]) {

    def |=>[Res2](mapFn: Res => Res2): (Req => M[Res2]) = req => monad.map(fn(req), mapFn)
    def |=+>[Res2](mapFn: (Req => Res => Res2)): (Req => M[Res2]) = req => monad.map(fn(req), mapFn(req))
    def |==+>[Res2](mapFn: (Req => Res => M[Res2])): (Req => M[Res2]) = req => monad.flatMap(fn(req), mapFn(req))
    def |=++>[Res2](mapFn: (Req => Res => Res => M[Res2])): (Req => M[Res2]) = { req => monad.flatMap(fn(req), { res: Res => mapFn(req)(res)(res) }) }
    def |==>[Res2](mapFn: Res => M[Res2]): (Req => M[Res2]) = req => monad.flatMap(fn(req), mapFn)
  }


  implicit class MonadCanFailFunctionPimper[M[_], Req, Res](fn: Req => M[Res]) {
    def |=|+>[Fail, Res2](mapFn: (Req => Res => Either[Fail, Res2]))(implicit monad: MonadCanFail[M, Fail]): (Req => M[Res2]) = req =>
      monad.flatMap[Res, Res2](fn(req), res => mapFn(req)(res) match {
        case Left(f) => monad.fail(f)
        case Right(t) => monad.liftM(t)
      })
    def |=|>[Fail, Res2](mapFn: Res => Either[Fail, Res2])(implicit monad: MonadCanFail[M, Fail]): Req => M[Res2] = { req: Req =>
      fn(req).flatMap(res => mapFn(res).fold[M[Res2]]({ fail: Fail => monad.fail(fail) }, { res2: Res2 => monad.liftM(res2) }))
    }

  }

  implicit class MonadWithExceptionFunctionPimper[M[_], Req, Res](fn: Req => M[Res])(implicit monad: MonadWithException[M]) {
    def onEnterAndExitM[Mid](mid: Req => Mid, after: Mid => Try[Res] => Unit): Req => M[Res] = { req: Req =>
      withValue(mid(req))(m => Exceptions(fn(req)).registerSideeffect(after(m)))
    }
  }

  implicit class MonadWithCanFailAndExceptionFunctionPimper[M[_], Req, Res](fn: Req => M[Res]) {
    def sideEffectWithReq[Fail](mapFn: Req => Try[Either[Fail, Res]] => Unit)(implicit monad: MonadCanFailWithException[M, Fail]): Req => M[Res] = { req: Req => Exceptions(fn(req)).onComplete[Fail](mapFn(req)) }
  }

}
