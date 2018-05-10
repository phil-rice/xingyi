package one.xingyi.core.language

import one.xingyi.core.functions.Monad
import one.xingyi.core.language.Language.sideeffect
import scala.language.higherKinds

object FunctionLanguage extends FunctionLanguage
trait FunctionLanguage {

  implicit class FunctionPimper[Req, Res](fn: Req => Res) {
    def liftFn[M[_]](implicit monad: Monad[M]) = { req: Req => monad.liftM(fn(req)) }
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
    def onExit[Mid](mid: Req => Mid, after: (Mid, Res) => Unit) = { req: Req =>
      val m = mid(req)
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

    def ~~>[M[_], Res2](fn2: Res => M[Res2])(implicit monad: Monad[M]): Req => M[Seq[Res2]] = { req: Req => monad.flattenM(fn(req).map(fn2)) }
    def ~+>[M[_], Res2](fn2: Res => M[Res2])(implicit monad: Monad[M]): Req => M[Seq[(Res, Res2)]] = { req: Req => monad.flattenM(fn(req).map(r => monad.map[Res2, (Res, Res2)](fn2(r), res2 => (r, res2)))) }
  }


}
