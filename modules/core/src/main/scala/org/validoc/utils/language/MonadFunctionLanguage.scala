package org.validoc.utils.language

import org.validoc.utils.exceptions.Exceptions
import org.validoc.utils.functions.{Monad, MonadCanFail, MonadCanFailWithException, MonadWithException}
import org.validoc.utils.language.Language.withValue

import scala.util.Try
import scala.language._

trait MonadFunctionLanguage extends MonadLanguage {

  implicit class MonadFunctionPimper[M[_], Req, Res](fn: Req => M[Res])(implicit monad: Monad[M]) {

    def |=>[Res2](mapFn: Res => Res2): (Req => M[Res2]) = req => monad.map(fn(req), mapFn)
    def |=+>[Res2](mapFn: (Req => Res => Res2)): (Req => M[Res2]) = req => monad.map(fn(req), mapFn(req))
    def |==+>[Res2](mapFn: (Req => Res => M[Res2])): (Req => M[Res2]) = req => monad.flatMap(fn(req), mapFn(req))
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
    def |=|>[Fail, Res2](mapFn: Res => Either[Fail, Res2])(implicit monad: MonadCanFail[M, Fail]): Req => M[Res2] = { req: Req =>
      fn(req).flatMap(res => mapFn(res).fold[M[Res2]]({ fail: Fail => monad.fail(fail) }, { res2: Res2 => monad.liftM(res2) }))
    }

  }

  implicit class MonadWithExceptionFunctionPimper[M[_], Req, Res](fn: Req => M[Res])(implicit monad: MonadWithException[M]) {
    def foldException[Res2](fnThrowable: Throwable => M[Res2], fnMap: Res => M[Res2]): (Req => M[Res2]) = { req => monad.foldException[Res, Res2](fn(req), fnThrowable, fnMap) }
    def onEnterAndExitM[Mid](mid: Req => Mid, after: Mid => Try[Res] => Unit): Req => M[Res] = { req: Req =>
      withValue(mid(req))(m => fn(req).registerSideeffect(after(m)))
    }
  }

  implicit class MonadWithCanFailAndExceptionFunctionPimper[M[_], Req, Res](fn: Req => M[Res]) {
    def sideEffectWithReq[Fail](mapFn: Req => Try[Either[Fail, Res]] => Unit)(implicit monad: MonadCanFailWithException[M, Fail]): Req => M[Res] = { req: Req => Exceptions(fn(req)).onComplete[Fail](mapFn(req)) }

  }

  implicit class MonadSeqFunctionPimper[M[_], Req, Res](fn: Req => M[Seq[Res]])(implicit async: Monad[M]) {
    def |*=>[Res2](mapFn: Res => Res2): (Req => M[Seq[Res2]]) = { req => async.map[Seq[Res], Seq[Res2]](fn(req), _.map(mapFn)) }
    def |*==>[Res2](mapFn: Res => M[Res2]): (Req => M[Seq[Res2]]) = { req =>
      async.flatMap[Seq[Res], Seq[Res2]](fn(req), { seqRes => async.flattenM(seqRes.map(mapFn))
      })
    }
  }

}
