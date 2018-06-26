package one.xingyi.core.functions

trait SemiGroup[T] {
  def add(one: T, two: T): T

  //  def add(head: T, tail: List[T]): T = tail.foldLeft(head)(add)
}

object SemiGroup {
  implicit def forInt: SemiGroup[Int] = { (t1, t2) => t1 + t2 }
  implicit def forString: SemiGroup[String] = { (t1, t2) => t1 + t2 }
  implicit def forSeq[T]: SemiGroup[Seq[T]] = { (t1, t2) => t1 ++ t2 }
  implicit def forList[T]: SemiGroup[List[T]] = { (t1, t2) => t1 ++ t2 }
  implicit def forMap[K,V]: SemiGroup[Map[K,V]] = { (t1, t2) => t1 ++ t2 }
  implicit def forOption[T]: SemiGroup[Option[T]] = { (t1, t2) => t1.fold(t2)(_ => t1) }
  implicit def forPartialFn[P, R]: SemiGroup[PartialFunction[P, R]] = { (t1, t2) => t1 orElse t2 }
}
object SemiGroupLanguage extends SemiGroupLanguage
trait SemiGroupLanguage {
  implicit class SemiGroupops[T](t: T)(implicit semiGroup: SemiGroup[T]) {
    def |+|(t1: T): T = semiGroup.add(t, t1)
    def or(t1: T): T = semiGroup.add(t, t1)
  }
  implicit class SeqOfSemiGroups[T](ts: Seq[T])(implicit semiGroup: SemiGroup[T]) {
    def orAll = ts.reduce(semiGroup.add)
  }
}

trait Zero[T] {
  def zero: T
}

object Zero {
  implicit def forInt: Zero[Int] = new Zero[Int] {def zero = 0}
  implicit def forString: Zero[String] = new Zero[String] {def zero = ""}
  implicit def forList[T]: Zero[List[T]] = new Zero[List[T]] {def zero = List()}
  implicit def zeroForSeq[T] = new Zero[Seq[T]] {def zero: List[T] = List()}
  implicit def zeroForMap[K, V] = new Zero[Map[K, V]] {override def zero = Map()}
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