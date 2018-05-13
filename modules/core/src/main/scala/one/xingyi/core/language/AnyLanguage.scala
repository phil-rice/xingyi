package one.xingyi.core.language

import one.xingyi.core.functions._
import one.xingyi.core.monad._

import scala.util.Try
import scala.language.higherKinds
import scala.reflect.ClassTag

object AnyLanguage extends AnyLanguage
trait AnyLanguage {

  def toSome[X](x: X): Option[X] = Some(x)

  def withValue[X, Y](x: X)(fn: X => Y) = fn(x)

  def sideeffect[X, Y](x: X)(fn: X => Y) = {
    fn(x)
    x
  }
  implicit class AnyPimper[T](t: => T) {
    def |>[T2](fn: T => T2) = fn(t)
    def liftM[M[_]](implicit container: Liftable[M]): M[T] = container.liftM(t)
    def |+>[T1](fn: T => T => T1): T1 = fn(t)(t)
    def liftException[M[_], T1](implicit async: MonadWithException[M], ev: T <:< Throwable): M[T1] = async.exception(t)
    def |?[M[_] : Functor, Failure](validation: T => Seq[Failure])(implicit withFailure: MonadCanFail[M, Failure], monoid: Monoid[Failure]): M[T] =
      validation(t) match {
        case Nil => t.liftM
        case s => withFailure.fail(monoid.addAll(s))
      }
    def ifError(fn: Exception => T): T = try {
      t
    } catch {
      case e: Exception => fn(e)
    }

    def sideeffect[X](block: T => Unit) = {
      val result = t
      block(result)
      result
    }
    def sideeffectIfIs[Sub <: T](fn: Sub => Unit)(implicit classTag: ClassTag[Sub]) = if (classTag.runtimeClass.isAssignableFrom(t.getClass)) fn(t.asInstanceOf[Sub])
  }

  implicit class BooleanPimper(boolean: Boolean) {
    def toOption[T](value: => T): Option[T] = if (boolean) Some(value) else None
  }


  implicit class TryPimper[T](tryT: Try[T]) {
    def liftTry[M[_]](implicit monadWithException: MonadWithException[M]): M[T] = tryT.fold(monadWithException.exception, monadWithException.liftM)
  }
}
