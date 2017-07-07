package org.validoc.utils.arrows

import org.validoc.utils.concurrency.Async
import org.validoc.utils.monads.Monad

import scala.language.higherKinds

trait Arrows {

  implicit class AnyPimper[T](t: T) {
    def ~>[T2](fn: T => T2) = fn(t)
  }

  implicit class AsyncPimper[M[_], T](m: M[T])(implicit monad: Monad[M]) {
    def ~>[T2](fn: T => T2): M[T2] = monad.map(m, fn)

    def ~*>[T2](fn: T => M[T2]): M[T2] = monad.flatMap(m, fn)
  }

  implicit class MonadSeqPimper[M[_], T](seq: M[Seq[T]])(implicit async: Async[M]) {
    def ~>[T2](fn: T => T2) = async.map[Seq[T], Seq[T2]](seq, _.map(fn))

    def ~*>[T2](fn: T => M[T2]) = async.flatMap[Seq[T], Seq[T2]](seq, x => async.join(x.map(fn)))
  }


  implicit class MonadFunctionPimper[M[_], Req, Res](fn: Req => M[Res])(implicit async: Async[M]) {
    def ~~>[Res2](mapFn: Res => Res2): (Req => M[Res2]) = req => async.map(fn(req), mapFn)

    def ~~*>[Res2](mapFn: Res => M[Res2]): (Req => M[Res2]) = req => async.flatMap(fn(req), mapFn)

    def split[Res1, Res2](mapFn1: Res => M[Res1], mapFn2: Res => M[Res2]): (Req => M[(Res1, Res2)]) = { req =>
      val res = fn(req)
      val res1 = async.flatMap(res, mapFn1)
      val res2 = async.flatMap(res, mapFn2)
      async.join2(res1, res2)
    }

    def tee[Res1](mapFn2: Res => M[Res1]): (Req => M[(Res, Res1)]) = { req =>
      val res = fn(req)
      val res2 = async.flatMap(res, mapFn2)
      async.join2(res, res2)
    }


  }

  implicit class MonadSeqFunctionPimper[M[_], Req, Res](fn: Req => M[Seq[Res]])(implicit async: Async[M]) {
    def ~~>[Res2](mapFn: Res => Res2): (Req => M[Seq[Res2]]) = { req => async.map[Seq[Res], Seq[Res2]](fn(req), _.map(mapFn)) }

    def ~~*>[Res2](mapFn: Res => M[Res2]): (Req => M[Seq[Res2]]) = { req =>
      async.flatMap[Seq[Res], Seq[Res2]](fn(req), { seqRes => async.join(seqRes.map(mapFn))
      })
    }
  }


  implicit class FunctionPimper[Req, Res](fn: Req => Res) {
    def ~>[Res2](fn2: Res => Res2): (Req) => Res2 = { res: Req => fn2(fn(res)) }
  }

  implicit class SeqFunctionPimper[Req, Res](fn: Req => Seq[Res]) {
    def ~>[Res2](fn2: Res => Res2): (Req) => Seq[Res2] = { res: Req => fn(res).map(fn2) }

    def ~~>[M[_], Res2](fn2: Res => M[Res2])(implicit async: Async[M]): Req => M[Seq[Res2]] = { res: Req => async.join(fn(res).map(fn2)) }
  }

  implicit class Tuple2MonadPimper[M[_], T1, T2](tuple: (M[T1], M[T2]))(implicit async: Async[M]) {
    def join[T](fn: (T1, T2) => T): M[T] = async.map(async.join2(tuple._1, tuple._2), fn.tupled)

  }

  implicit class TupleFn2MonadPimper[M[_], Req, Res1, Res2](tuple: (Req => M[Res1], Req => M[Res2]))(implicit async: Async[M]) {
    def join[T](fn: (Res1, Res2) => T): Req => M[T] = { req => async.map(async.join2(tuple._1(req), tuple._2(req)), fn.tupled) }

  }

}

object Arrows extends Arrows
