package one.xingyi.utils.functions

import scala.concurrent.duration.Duration
import scala.language.higherKinds
import scala.util.Try

trait Async[M[_]] {
  def async[T](t: => T): M[T]
  def respond[T](m: M[T], fn: Try[T] => Unit): M[T]
  def await[T](m: M[T]): T
  def delay[T](duration: Duration)(block: => M[T]): M[T]
//  def convert[M1[_], T](m: M1[T]): M1[T]
}

trait MonadConvertor[M1[_], M2[T]] {
  def apply[T](m1: M1[T]): M2[T]
}


