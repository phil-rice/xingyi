/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.monad

import java.util.concurrent.atomic.AtomicInteger

import scala.collection.concurrent.TrieMap
import scala.language.higherKinds

trait Liftable[M[_]] {
  def liftM[T](t: T): M[T]

}
trait Functor[M[_]] extends Liftable[M] {
  def map[T, T1](m: M[T], fn: T => T1): M[T1]
}

trait Monad[M[_]] extends Functor[M] {
  def flatMap[T, T1](m: M[T], fn: T => M[T1]): M[T1]
  def flattenM[T](ms: Seq[M[T]]): M[Seq[T]] = ms.foldLeft(liftM(Seq[T]())) { (mAcc: M[Seq[T]], mV: M[T]) => flatMap(mAcc, (acc: Seq[T]) => flatMap(mV, (v: T) => liftM(acc :+ v))) }
  def flattenListM[T](ms: List[M[T]]): M[List[T]] = ms.foldLeft(liftM(List[T]())) { (mAcc: M[List[T]], mV: M[T]) => flatMap(mAcc, (acc: List[T]) => flatMap(mV, (v: T) => liftM(acc :+ v))) }
}

trait MonadWithException[M[_]] extends Monad[M] {
  def exception[T](t: Throwable): M[T]
  def recover[T](m: M[T], fn: Throwable => M[T]): M[T]
}

trait LiftFailure[M[_], Fail] {
  def fail[T](f: Fail): M[T]
}

trait MonadCanFail[M[_], Fail] extends Monad[M] with LiftFailure[M, Fail] {
  def flatMapEither[T, T1](m: M[T], fn: Either[Fail, T] => M[T1]): M[T1]
}

object MonadCanFail {
  implicit def monadCanFailForEither[Fail]: MonadCanFail[({type λ[α] = Either[Fail, α]})#λ, Fail] = new MonadCanFailForEither[Fail]
}

trait MonadCanFailWithException[M[_], Fail] extends MonadWithException[M] with MonadCanFail[M, Fail] {
  def foldWithExceptionAndFail[T, T1](m: M[T], fnE: Throwable => M[T1], fnFailure: Fail => M[T1], fn: T => M[T1]): M[T1]
}

class MonadCanFailForEither[Fail] extends MonadCanFail[({type λ[α] = Either[Fail, α]})#λ, Fail] {
  type M[T] = Either[Fail, T]
  override def fail[T](f: Fail): Either[Fail, T] = Left(f)
  override def liftM[T](t: T): Either[Fail, T] = Right(t)
  override def flatMap[T, T1](m: Either[Fail, T], fn: T => M[T1]): M[T1] = m.right.flatMap(fn)
  override def map[T, T1](m: Either[Fail, T], fn: T => T1): M[T1] = m.right.map(fn)
  override def flatMapEither[T, T1](m: Either[Fail, T], fn: Either[Fail, T] => M[T1]): Either[Fail, T1] = fn(m)
}


trait MonadWithState[M[_]] extends Monad[M] {
  def mapState[V, T, T1](m: M[T], localVariable: LocalVariable[V], fn: Seq[V] => T1): M[T1]
  def mapWith[V, T, T1](m: M[T], localVariable: LocalVariable[V], fn: (T, Seq[V]) => T1): M[T1]
  def putInto[V, T](localVariable: LocalVariable[V], t: V)(m: M[T]): M[T]
  def liftMAndPut[T, V](t: T, localVariable: LocalVariable[V], v: V): M[T] = putInto(localVariable, v)(liftM(t))
  //OK this sucks. I would like to do it better
  //It returns a new monad with a cleared local variable... but potentially when you flat map with it, the previous threads stuff
  //will be carried across.
  //However if you call this and use this as the root of your process tree it should all work.
  //The behavior is undefined if you do  mWithState.flatMap(_ => clear). So don't do that!
  def clear: M[Unit]
}

trait SimpleMonadWithState[M[_]] extends MonadWithState[M] {
  def localVariables = new InheritableThreadLocal[Map[LocalVariable[_], Seq[Any]]]() // is this a higher ranked type? The _ and the Any are the same

  //// the classical but unpleasant workaround for the higher ranked type.
  private def get[T1, T, V](localVariable: LocalVariable[V]) = localVariables.get().getOrElse(localVariable, Seq()).asInstanceOf[Seq[V]]

  def mapState[V, T, T1](m: M[T], localVariable: LocalVariable[V], fn: Seq[V] => T1): M[T1] =
    map[T, T1](m, t => fn(get(localVariable)))

  def mapWith[V, T, T1](m: M[T], localVariable: LocalVariable[V], fn: (T, Seq[V]) => T1): M[T1] =
    map[T,T1](m, t => fn(t, get(localVariable)))
  def putInto[V, T](localVariable: LocalVariable[V], v: V)(m: M[T]): M[T] =
    map[T, T](m, { t => localVariables.set(localVariables.get + (localVariable -> (get(localVariable) :+ v))); t })

  override def clear: M[Unit] = {localVariables.set(Map()); liftM(())}
}

object LocalVariable {
  private val nextIndex = new AtomicInteger()
  def apply[V]() = new LocalVariable[V](nextIndex.getAndIncrement())
}

class LocalVariable[V](val offset: Int) {
  //this is safe, and is the price for not having horrible type signatures polluting every method
  def getFrom(state: Map[Int, Seq[Any]]): Seq[V] = state.getOrElse(offset, Nil).asInstanceOf[Seq[V]]
}
