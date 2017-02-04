package org.validoc.utils.monads

import scala.util.Try

trait FlatMap[M[_]] {
  def flatMap[T, T2](m: M[T], fn: T => M[T2]): M[T2]
}


trait Monad[M[_]] extends FlatMap[M] {
  def lift[T](t: => T): M[T]

  /** This may just throw the exception if it's not meaningfull to lift it. Meaningful monads include Try, Future ... */
  def liftTry[T](tryT: Try[T]): M[T]

  def map[T, T2](m: M[T], fn: T => T2): M[T2]

}


