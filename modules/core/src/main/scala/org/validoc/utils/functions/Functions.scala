package org.validoc.utils.functions

import org.validoc.utils.monads.FlatMap
import org.validoc.utils.service.WrappedTypes
import scala.language.higherKinds

object Functions {
  def pipelineFn[X](sideEffect: X => Unit)(x: X): X = {
    sideEffect(x)
    x
  }

  def print[P](msg: P => String): (P => P) = { p: P => println(msg(p)); p }


  def split[P, Temp, R](tempFn: P => Temp, fn: (Temp, P) => R) = { p: P => fn(tempFn(p), p) }

  implicit class FunctionPimper[Req, Res](fn: Req => Res) {
    def ~>[Res2](fn2: Res => Res2): (Req) => Res2 = { res: Req => fn2(fn(res)) }

    def ~~>[Res2](wrappedFunction: (Req => Res) => (Req => Res2)): (Req => Res2) = { req: Req => wrappedFunction(fn)(req) }

    def ~~~>[Req2, Res2](wrappedFunction: (Req => Res) => (Req2 => Res2)): (Req2 => Res2) = andThen(wrappedFunction)

    def andThen[Req2, Res2](wrappedFunction: (Req => Res) => (Req2 => Res2)): (Req2 => Res2) = { req: Req2 => wrappedFunction(fn)(req) }
  }


  implicit class KleisliPimper[M[_] : FlatMap, A, B](fn: A => M[B]) {
    import org.validoc.utils.concurrency.Async._
    def >=>[C](newFn: B => M[C]): A => M[C] = andThen(newFn)

    def andThen[C](newFn: B => M[C]): A => M[C] = fn(_).flatMap(newFn)
  }

}
