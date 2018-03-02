package org.validoc.utils

import org.validoc.utils.functions.Monoid

import scala.reflect.ClassTag

abstract class MonoidSpec[T](implicit monoid: Monoid[T], classTag: ClassTag[T]) extends UtilsSpec {

  behavior of s"Monoid[${classTag.runtimeClass.getSimpleName}]"

  def one: T

  def two: T

  def three: T

  import monoid._

  it should "be setup correctly" in {
    one shouldNot be(zero)
    one shouldNot be(two)
    one shouldNot be(three)
    two shouldNot be(zero)
    two shouldNot be(three)
    three shouldNot be(zero)
  }

  it should "obey the law that zero + X is X" in {
    add(one, zero) shouldBe one
    add(two, zero) shouldBe two
    add(three, zero) shouldBe three
  }
  it should "obey the law that X+ zero  is X" in {
    add(zero, one) shouldBe one
    add(zero, two) shouldBe two
    add(zero, three) shouldBe three
  }

  it should "obey (X+Y)+Z is X+(Y+Z)" in {
    add(add(one, two), three) shouldBe add(one, add(two, three))
  }

}

class MonoidForIntSpec extends MonoidSpec[Int] {
  override def one = 1

  override def two = 2

  override def three = 3
}
class MonoidForSeqSpec extends MonoidSpec[Seq[String]] {
  override def one = List("1")

  override def two = List("2")

  override def three = List("3")
}
