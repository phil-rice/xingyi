package org.validoc.utils.functions

trait SemiGroup[T] {
  def add(one: T, two: T): T

  def add(head: T, tail: List[T]): T = tail.foldLeft(head)(add)
}

object SemiGroup {
  implicit def semiGroupForInt[T] = new SemiGroup[Int] {
    override def add(one: Int, two: Int) = one + two
  }

  implicit def semiGroupForList[T] = new SemiGroup[List[T]] {
    override def add(one: List[T], two: List[T]): List[T] = one ::: two
  }

  implicit def semiGroupForMap[K, V] = new SemiGroup[Map[K, V]] {
    override def add(one: Map[K, V], two: Map[K, V]): Map[K, V] = one ++ two
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

  implicit def zeroForMap[K, V] = new Zero[Map[K, V]] {
    override def zero = Map()
  }
}


trait Monoid[T] {
  def zero: T

  def add(t1: T, t2: T): T
}

object Monoid {

  implicit class MonoidSeqPimper[T](t: Seq[T])(implicit monoid: Monoid[T]) {

    import monoid._

    def addAll: T = if (t.size < 1000)
      t.foldLeft(zero)(add)
    else
      monoid.add(t.toParArray.reduce(monoid.add), monoid.zero)
  }

  implicit def monoidFromSemiGroupAndZero[T: Zero : SemiGroup] = new Monoid[T] {
    override def zero: T = implicitly[Zero[T]].zero

    override def add(one: T, two: T): T = implicitly[SemiGroup[T]].add(one, two)
  }
}