/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.language
import one.xingyi.core.language.Language.sideeffect
import one.xingyi.core.monad.Monad

import scala.language.higherKinds

object FunctionLanguage extends FunctionLanguage
trait FunctionLanguage {
  implicit class FunctionFromMidToOptionOps[From, Mid, To](fn: From => Mid => Option[To]) {
    def orElse(fn2: From => Mid => Option[To]): From => Mid => Option[To] = { from: From => mid: Mid => fn(from)(mid).orElse(fn2(from)(mid)) }
    def orDefault(to: => To): From => Mid => To = { from: From => mid: Mid => fn(from)(mid).getOrElse(to) }
  }

  implicit class FunctionPimper[Req, Res](fn: Req => Res) {
    def liftFn[M[_]](implicit monad: Monad[M]) = { req: Req => monad.liftM(fn(req)) }
    def ~>[Res2](fn2: Res => Res2): (Req) => Res2 = { res: Req => fn2(fn(res)) }
    def ~^>(fn2: Res => Unit): (Req => Res) = { req: Req => sideeffect(fn(req))(fn2) }
    def ~+>[Res2](fn2: Req => Res => Res2): (Req => Res2) = { req: Req => fn2(req)(fn(req)) }
    def ~~+>[Res2](fn2: Res => Res => Res2): (Req => Res2) = { req: Req => val res = fn(req); fn2(res)(res) }
    //    def let[Mid, Res2](mid: Req => Mid)(fn2: Mid => Res => Res): Req => Res = { req: Req => fn2(mid(req))(fn(req)) }
    def onEnterAndExit[Mid](mid: Req => Mid, before: Mid => Unit, after: (Mid, Res) => Unit) = { req: Req =>
      val m = mid(req)
      before(m)
      val result = fn(req)
      after(m, result)
      result
    }
    def onExit[Mid](mid: Req => Mid, after: (Mid, Res) => Unit) = { req: Req =>
      val m = mid(req)
      val result = fn(req)
      after(m, result)
      result
    }
  }

  implicit class OptionFunctionOps[T1, T2](fn: T1 => Option[T2]) {
    def ~?>[T3](fn2: T2 => T3): T1 => Option[T3] = { t1: T1 => fn(t1).map(fn2) }
    def ~?^>[T3](fn2: T2 => T3): T1 => Option[T2] = { t1: T1 => fn(t1).map { t2 => fn2(t2); t2 } }
    def ~+?>[T3](fn2: T1 => T2 => T3): T1 => Option[T3] = { t1: T1 => fn(t1).map(fn2(t1)) }
    def ~~+?>[T3](fn2: T1 => T2 => Option[T3]): T1 => Option[T3] = { t1: T1 => fn(t1).flatMap(fn2(t1)) }
    def ~~?>[T3](fn2: T2 => Option[T3]): T1 => Option[T3] = { t1: T1 => fn(t1).flatMap(fn2) }
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

    def ~~>[M[_], Res2](fn2: Res => M[Res2])(implicit monad: Monad[M]): Req => M[Seq[Res2]] = { req: Req => monad.flattenM(fn(req).map(fn2)) }
    def ~+>[M[_], Res2](fn2: Res => M[Res2])(implicit monad: Monad[M]): Req => M[Seq[(Res, Res2)]] = { req: Req => monad.flattenM(fn(req).map(r => monad.map[Res2, (Res, Res2)](fn2(r), res2 => (r, res2)))) }
  }


}
