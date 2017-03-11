package org.validoc.utils.monads

import org.validoc.utils.concurrency.Async._

import scala.language.higherKinds

object Kleisli {

  implicit class KleisliPimper[M[_] : FlatMap, A, B](fn: A => M[B]) {
    def >=>[C](newFn: B => M[C]): A => M[C] = andThen(newFn)

    def andThen[C](newFn: B => M[C]): A => M[C] = fn(_).flatMap(newFn)

  }

}

