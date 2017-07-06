package org.validoc.utils.functions

trait SemiGroup[T] {
  def add(one: T, two: T): T

  def add(head: T, tail: List[T]): T = tail.foldLeft(head)(add)
}

object SemiGroup {
  implicit def semiGroupForList[T] = new SemiGroup[List[T]] {
    override def add(one: List[T], two: List[T]): List[T] = one ::: two
  }

}

trait Zero[T] {
  def zero: T
}

object Zero {

  implicit object ZeroForInt extends Zero[Int] {
    override def zero: Int = 0
  }

  implicit def zeroForList[T] = new Zero[List[T]] {
    override def zero: List[T] = List()
  }
}

trait Monoid[T] extends SemiGroup[T] with Zero[T] {
  def addAll(seq: Iterable[T]) = seq.foldLeft(zero)(add)
}

object Monoid {

  implicit class MonoidSeqPimper[T](t: Iterable[T])(implicit monoid: Monoid[T]) {
    def addAll = monoid.addAll(t)
  }

  implicit def monoidFromSemiGroupAndZero[T: Zero : SemiGroup] = new Monoid[T] {
    override def zero: T = implicitly[Zero[T]].zero

    override def add(one: T, two: T): T = implicitly[SemiGroup[T]].add(one, two)
  }

}