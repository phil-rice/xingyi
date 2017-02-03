package org.validoc.utils.monads

object Kleisli {

  implicit class KleisliPimper[M[_] : FlatMap, A, B](fn: A => M[B]) {
    def ==>[C](newFn: B => M[C]): A => M[C] = andThen(newFn)

    def andThen[C](newFn: B => M[C]): A => M[C] = { a => implicitly[FlatMap[M]].flatMap(fn(a), newFn) }
  }

}

