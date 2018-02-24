package org.validoc.utils.functions

import org.validoc.utils.containers.Monad

import scala.language.higherKinds
object Functions {
  def pipelineFn[X](sideEffect: X => Unit)(x: X): X = {
    sideEffect(x)
    x
  }

  def print[P](msg: P => String): (P => P) = { p: P => println(msg(p)); p }


  def split[P, Temp, R](tempFn: P => Temp, fn: (Temp, P) => R) = { p: P => fn(tempFn(p), p) }

//  implicit class FunctionPimper[Req, Res](fn: Req => Res) {
//    def ~>[Res2](fn2: Res => Res2): (Req) => Res2 = { res: Req => fn2(fn(res)) }
//
//    def ~~>[Res2](wrappedFunction: (Req => Res) => (Req => Res2)): (Req => Res2) = { req: Req => wrappedFunction(fn)(req) }
//
//    def ~~~>[Req2, Res2](wrappedFunction: (Req => Res) => (Req2 => Res2)): (Req2 => Res2) = andThen(wrappedFunction)
//
//    def andThen[Req2, Res2](wrappedFunction: (Req => Res) => (Req2 => Res2)): (Req2 => Res2) = { req: Req2 => wrappedFunction(fn)(req) }
//  }



}
