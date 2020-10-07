/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.language

import one.xingyi.core.functions._
import one.xingyi.core.monad._

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

object FunctorLanguage extends FunctorLanguage
trait FunctorLanguage {

  implicit class FunctorPimper[M[_], T](m: M[T])(implicit functor: Functor[M]) {
    def map[T1](fn: T => T1): M[T1] = functor.map(m, fn)

    def forEach(fn: T => Unit): M[T] = functor.map(m, (t: T) => {
      fn(t);
      t
    })

    def |=>[T2](fn: T => T2): M[T2] = functor.map(m, fn)

    def |+>[T1](fn: T => T => T1): M[T1] = functor.map[T, T1](m, t => fn(t)(t))
  }

}

object MonadLanguage extends MonadLanguage

trait MonadLanguage extends FunctorLanguage {
  def join2[M[_], T1, T2](m1: M[T1], m2: M[T2])(implicit m: Monad[M]): M[(T1, T2)] = m.flatMap(m1, (t1: T1) => m.flatMap(m2, (t2: T2) => m.liftM((t1, t2))))

  def join3[M[_], T1, T2, T3](m1: M[T1], m2: M[T2], m3: M[T3])(implicit m: Monad[M]): M[(T1, T2, T3)] = m.flatMap(m1, (t1: T1) => m.flatMap(m2, (t2: T2) => m.flatMap(m3, (t3: T3) => m.liftM((t1, t2, t3)))))

  def join4[M[_], T1, T2, T3, T4](m1: M[T1], m2: M[T2], m3: M[T3], m4: M[T4])(implicit m: Monad[M]): M[(T1, T2, T3, T4)] = m.flatMap(m1, (t1: T1) => m.flatMap(m2, (t2: T2) => m.flatMap(m3, (t3: T3) => m.flatMap(m4, (t4: T4) => m.liftM((t1, t2, t3, t4))))))

  def join5[M[_], T1, T2, T3, T4, T5](m1: M[T1], m2: M[T2], m3: M[T3], m4: M[T4], m5: M[T5])(implicit m: Monad[M]): M[(T1, T2, T3, T4, T5)] = m.flatMap(m1, (t1: T1) => m.flatMap(m2, (t2: T2) => m.flatMap(m3, (t3: T3) => m.flatMap(m4, (t4: T4) => m.flatMap(m5, (t5: T5) => m.liftM((t1, t2, t3, t4, t5)))))))


  implicit class MonadPimper[M[_], T](m: M[T])(implicit monad: Monad[M]) {
    def flatMap[T1](fn: T => M[T1]): M[T1] = monad.flatMap(m, fn)

    def |=*>[T1](fn: T => Seq[M[T1]]): M[Seq[T1]] = monad.flatMap(m, { req: T => monad.flattenM(fn(req)) })

    def |=+>[T1](fn: T => T => M[T1]): M[T1] = monad.flatMap(m, { t: T => fn(t)(t) })
  }


  def join2WithReq[M[_], Req, Res1, Res2](firstService: Req => M[Res1], secondService: Req => M[Res2])(implicit monad: Monad[M]): Req => M[(Req, Res1, Res2)] = { req: Req => join3(monad.liftM(req), firstService(req), secondService(req)) }

  def join3WithReq[M[_], Req, Res1, Res2, Res3](firstService: Req => M[Res1], secondService: Req => M[Res2], thirdService: Req => M[Res3])(implicit monad: Monad[M]): Req => M[(Req, Res1, Res2, Res3)] = { req: Req => join4(monad.liftM(req), firstService(req), secondService(req), thirdService(req)) }

  def join4WithReq[M[_], Req, Res1, Res2, Res3, Res4](firstService: Req => M[Res1], secondService: Req => M[Res2], thirdService: Req => M[Res3], fouthService: Req => M[Res4])(implicit monad: Monad[M]): Req => M[(Req, Res1, Res2, Res3, Res4)] = { req: Req => join5(monad.liftM(req), firstService(req), secondService(req), thirdService(req), fouthService(req)) }

  implicit class MonadWithExceptionPimper[M[_], T](m: M[T])(implicit monad: MonadWithException[M]) {
    def registerSideeffect(fn: Try[T] => Unit): M[T] = monad.flatMap[T, T](monad.recover(m, { e => fn(Failure(e)); monad.exception[T](e) }), { x => fn(Success(x)); monad.liftM(x) })
    def toSuccessOrFail[S[_]](implicit s: SuccessOrFail[S]): M[S[T]] = monad.toSucessFail[S, T](m)
  }
  implicit class MonadWithExceptionSeqPimper[M[_], T](seq: Seq[M[T]])(implicit monad: MonadWithException[M]) {
    def flattenToSuccessFail[S[_] : SuccessOrFail]: M[Seq[S[T]]] =
      monad.flattenM(seq.map(monad.toSucessFail[S, T]))

  }

  implicit class MonadOfSucessFailPimper[M[_], S[_], T](m: M[S[T]])(implicit monadWithException: MonadWithException[M], s: SuccessOrFail[S]) {
    def fold[Res](error: Throwable => Res, fn: T => Res) = m.map(s.fold(_, error, fn))
  }
  implicit class MonadCanFailWithExceptionPimper[M[_], T](m: M[T]) {
    def onComplete[Fail](fn: Try[Either[Fail, T]] => Unit)(implicit monad: MonadCanFailWithException[M, Fail]): M[T] = monad.foldWithExceptionAndFail[T, T](m,
      { e: Throwable => fn(Failure(e)); monad.exception(e) },
      { f: Fail => fn(Success(Left(f))); monad.fail(f) },
      { t: T =>
        val value = Success(Right(t))
        fn(value);
        monad.liftM(t)
      }
    )

    def mapTryFail[Fail, T1](fn: Try[Either[Fail, T]] => M[T1])(implicit monad: MonadCanFailWithException[M, Fail]): M[T1] = monad.foldWithExceptionAndFail[T, T1](m,
      t => fn(Failure(t)),
      f => fn(Success(Left(f))),
      t => fn(Success(Right(t))))
  }

  implicit class MonadWithStatePimper[M[_], T](m: M[T])(implicit monad: MonadWithState[M]) {
    def mapWith[V, T1](localVariable: LocalVariable[V])(fn: (T, Seq[V]) => T1): M[T1] = monad.mapWith(m, localVariable, fn)

    def mapState[V, T1](localVariable: LocalVariable[V])(fn: Seq[V] => T1): M[T1] = monad.mapState(m, localVariable, fn)

    def putInto[V](localVariable: LocalVariable[V], t: V): M[T] = monad.putInto(localVariable, t)(m)
  }

}







