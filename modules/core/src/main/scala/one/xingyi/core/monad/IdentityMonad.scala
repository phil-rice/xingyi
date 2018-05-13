package one.xingyi.core.monad
import one.xingyi.core.functions.Functions
import one.xingyi.core.map.Maps

import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}


case class IdentityMonad[T](value: Try[T], state: Map[Int, Seq[Any]]) {
  def overwritingStateIn[T1](old: IdentityMonad[T1]) = IdentityMonad(value, Maps.mergeAll(Seq(old.state, state)))
}

object IdentityMonad {

  implicit object MonadForIdentityMonad extends MonadWithState[IdentityMonad] with MonadCanFailWithException[IdentityMonad, Throwable] with Async[IdentityMonad] {
    override def async[T](t: => T): IdentityMonad[T] = Try(t).fold(exception, liftM)
    override def await[T](m: IdentityMonad[T]): T = m.value.fold(e => throw e, Functions.identity)
    override def delay[T](duration: Duration)(block: => IdentityMonad[T]): IdentityMonad[T] = block
    override def liftM[T](t: T): IdentityMonad[T] = IdentityMonad(Success(t), Map())

    override def map[T, T1](m: IdentityMonad[T], fn: T => T1): IdentityMonad[T1] = IdentityMonad[T1](m.value.map(fn), m.state)
    override def flatMap[T, T1](m: IdentityMonad[T], fn: T => IdentityMonad[T1]): IdentityMonad[T1] = m.value match {
      case Success(t) => IdentityMonad(Try(fn(t).value).fold[Try[T1]](x => Failure(x), x => x), m.state)
      case Failure(t) => IdentityMonad(Failure(t), m.state)
    }
    override def flatMapEither[T, T1](m: IdentityMonad[T], fn: Either[Throwable, T] => IdentityMonad[T1]): IdentityMonad[T1] = m.value.fold(t => fn(Left(t)), t => fn(Right(t))).overwritingStateIn(m)
    override def mapState[V, T, T1](m: IdentityMonad[T], localVariable: LocalVariable[V], fn: Seq[V] => T1): IdentityMonad[T1] = IdentityMonad(Try(fn(localVariable.getFrom(m.state))), m.state)
    override def mapWith[V, T, T1](m: IdentityMonad[T], localVariable: LocalVariable[V], fn: (T, Seq[V]) => T1): IdentityMonad[T1] = map[T, T1](m, t => fn(t, localVariable.getFrom(m.state)))

    override def foldWithExceptionAndFail[T, T1](m: IdentityMonad[T], fnE: Throwable => IdentityMonad[T1], fnFailure: Throwable => IdentityMonad[T1], fn: T => IdentityMonad[T1]): IdentityMonad[T1] =
      m.value match {
        case Failure(t) => fnE(t).overwritingStateIn(m)
        case Success(t) => fn(t).overwritingStateIn(m)
      }
    override def respond[T](m: IdentityMonad[T], fn: Try[T] => Unit): IdentityMonad[T] = {
      fn(m.value)
      m
    }
    override def fail[T](f: Throwable): IdentityMonad[T] = IdentityMonad(Failure(f), Map())
    override def exception[T](t: Throwable): IdentityMonad[T] = IdentityMonad(Failure(t), Map())
    override def recover[T](m: IdentityMonad[T], fn: Throwable => IdentityMonad[T]): IdentityMonad[T] = m.value.fold(fn, _ => m)
    override def putInto[V, T](localVariable: LocalVariable[V], t: V)(m: IdentityMonad[T]): IdentityMonad[T] = IdentityMonad(m.value, Maps.addTo(m.state, localVariable.offset, t))
  }

}