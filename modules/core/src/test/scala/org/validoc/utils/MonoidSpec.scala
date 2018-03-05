package org.validoc.utils

import org.validoc.utils.functions.{Monoid, SemiGroup, Zero}

import scala.reflect.ClassTag

trait ZeroSpec[T] extends UtilsSpec {
  def classTag: ClassTag[T]
  def zero: Zero[T]

  def zeroValue: T

  behavior of s"Zero[${classTag.runtimeClass.getSimpleName}]"

  it should "have a zero" in {
    zero.zero shouldBe zeroValue
  }

}

trait SemiGroupSpec[T] extends UtilsSpec {
  def classTag: ClassTag[T]
  def semiGroup: SemiGroup[T]

  behavior of s"Monoid[${classTag.runtimeClass.getSimpleName}]"

  def zeroValue: T

  def one: T

  def two: T

  def three: T

  it should "be setup correctly" in {
    one shouldNot be(zeroValue)
    one shouldNot be(two)
    one shouldNot be(three)
    two shouldNot be(zeroValue)
    two shouldNot be(three)
    three shouldNot be(zeroValue)
  }
  val sg = semiGroup

  import sg._

  it should "obey (1+2=3" in {
    add(one, two) shouldBe three
  }
  it should "obey (X+Y)+Z is X+(Y+Z)" in {
    add(add(one, two), three) shouldBe add(one, add(two, three))
  }

  it should "allow lists to be added" in {
    add(one, List(two)) shouldBe (three)
  }
}

abstract class MonoidSpec[T](implicit monoid: Monoid[T], val classTag: ClassTag[T]) extends SemiGroupSpec[T] with ZeroSpec[T] {

  override def zero = new Zero[T] {
    override def zero = monoid.zero
  }
  override def semiGroup = new SemiGroup[T] {
    override def add(one: T, two: T) = monoid.add(one, two)
  }


  behavior of s"Monoid[${classTag.runtimeClass.getSimpleName}]"


  import monoid._


  it should "obey the law that zero + X is X" in {
    add(one, zeroValue) shouldBe one
    add(two, zeroValue) shouldBe two
    add(three, zeroValue) shouldBe three
  }
  it should "obey the law that X+ zero  is X" in {
    add(zeroValue, one) shouldBe one
    add(zeroValue, two) shouldBe two
    add(zeroValue, three) shouldBe three
  }


}

class MonoidForIntSpec extends MonoidSpec[Int] {

  override def zeroValue = 0
  override def one = 1
  override def two = 2
  override def three = 3
}
class MonoidForSeqSpec extends MonoidSpec[Seq[String]] {
  override def zeroValue = List()
  override def one = List("1")
  override def two = List("2")
  override def three = List("1", "2")
}
class MonoidForMapSpec extends MonoidSpec[Map[Int, Int]] {
  override def zeroValue = Map()
  override def one = Map(1 -> 1)
  override def two = Map(2 -> 2)
  override def three = Map(1 -> 1, 2 -> 2)
}

