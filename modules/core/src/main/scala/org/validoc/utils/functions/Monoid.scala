package org.validoc.utils.functions

trait SemiGroup[T] {
  def add(one: T, two: T): T

  def add(head: T, tail: List[T]): T = tail.foldLeft(head)(add)
}

object SemiGroup {
  implicit def semiGroupForInt[T] = new SemiGroup[Int] {
    override def add(one: Int, two: Int) = one + two
  }
  implicit def semiGroupForString[T] = new SemiGroup[String] {
    override def add(one: String, two: String) = one + two
  }

  implicit def semiGroupForSeq[T] = new SemiGroup[Seq[T]] {
    override def add(one: Seq[T], two: Seq[T]): Seq[T] = one ++ two
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

  implicit object ZeroForString extends Zero[String] {
    override def zero: String = ""
  }

  implicit def zeroForSeq[T] = new Zero[Seq[T]] {
    override def zero: List[T] = List()
  }

  implicit def zeroForMap[K, V] = new Zero[Map[K, V]] {
    override def zero = Map()
  }
}


trait Monoid[T] {
  def zero: T

  def add(t1: T, t2: T): T
  def addAll(seq: Seq[T]) = if (seq.size < 1000) seq.foldLeft(zero)(add) else add(seq.toParArray.reduce(add), zero)
}

object Monoid {

  implicit class MonoidSeqPimper[T](t: Seq[T])(implicit monoid: Monoid[T]) {

    import monoid._

    def addAll: T = monoid.addAll(t)
  }

  implicit def monoidFromSemiGroupAndZero[T: Zero : SemiGroup] = new Monoid[T] {
    override def zero: T = implicitly[Zero[T]].zero

    override def add(one: T, two: T): T = implicitly[SemiGroup[T]].add(one, two)
  }
}