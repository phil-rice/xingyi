/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.language

import one.xingyi.core.exceptions.Exceptions
import one.xingyi.core.language.Language.withValue
import one.xingyi.core.monad.{Monad, MonadCanFail, MonadCanFailWithException, MonadWithException}

import scala.language._
import scala.util.Try

trait MonadFunctionLanguage extends MonadLanguage {

  implicit class MonadFunctionPimper[M[_], Req, Res](fn: Req => M[Res])(implicit monad: Monad[M]) {
    def |=>[Res2](mapFn: Res => Res2): (Req => M[Res2]) = req => monad.map(fn(req), mapFn)
    def |==>[Res2](mapFn: Res => M[Res2]): (Req => M[Res2]) = req => monad.flatMap(fn(req), mapFn)
    def |=+>[Res2](mapFn: (Req => Res => Res2)): (Req => M[Res2]) = req => monad.map(fn(req), mapFn(req))
    def |==+>[Res2](mapFn: (Req => Res => M[Res2])): (Req => M[Res2]) = req => monad.flatMap(fn(req), mapFn(req))
    def |=++>[Res2](mapFn: (Req => Res => Res => M[Res2])): (Req => M[Res2]) = { req => monad.flatMap(fn(req), { res: Res => mapFn(req)(res)(res) }) }
  }
  implicit class MonadFunctionOptPimper[M[_], Req, Res](fn: Req => M[Option[Res]])(implicit monad: Monad[M]) {
    def |?>[Res2](mapFn: Res => Res2): (Req => M[Option[Res2]]) = req => monad.map[Option[Res], Option[Res2]](fn(req), _.map(mapFn))
    def |??>[Res2](mapFn: Res => M[Res2]): (Req => M[Option[Res2]]) = req => monad.flatMap[Option[Res], Option[Res2]](fn(req), _.fold[M[Option[Res2]]](monad.liftM(None))(o => mapFn(o).map(Some(_))))
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
    def sideEffectWithReq[Fail](mapFn: Req => Try[Either[Fail, Res]] => Unit)(implicit monad: MonadCanFailWithException[M, Fail]): Req => M[Res] = { req: Req => Exceptions(fn(req)).onComplete(mapFn(req)) }
  }

}
