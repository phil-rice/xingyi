package org.validoc.utils.strings

import org.validoc.utils.{MonoidSpec, UtilsSpec}

class IndentSpec extends UtilsSpec {

  behavior of "IndentAndString"

  val oneTwo = List((1, "one"), (2, "two"))
  val threeFour = List((3, "three"), (4, "four"))
  it should "have a toString method with a marker" in {
    IndentAndString(3, oneTwo).toString("...", ",") shouldBe "...one,......two"
    IndentAndString(3, List()).toString("...", ",") shouldBe ""
  }

  it should "have a addLineAndIndent" in {
    IndentAndString(30, oneTwo).addLineAndIndent("new") shouldBe IndentAndString(31, oneTwo :+ ((30, "new")))
  }
  it should "have a insertLineAndIndent" in {
    IndentAndString(30, oneTwo).insertLineAndIndent("new") shouldBe IndentAndString(31, ((30, "new") :: oneTwo))
  }


  it should "have an unindent method that return the same lines but with depth reduced" in {
    IndentAndString(3, oneTwo).unindent shouldBe IndentAndString(2, oneTwo)
  }

  it should "invert the indent, which finds the biggest indent and makes that 0 . It leaes depth alone" in {
    IndentAndString(3, oneTwo).invertIndent shouldBe IndentAndString(3, List((1, "one"), (0, "two")))
  }

  it should "add offset to the indents " in {
    IndentAndString(3, oneTwo).offset(3) shouldBe IndentAndString(3, List((4, "one"), (5, "two")))
  }

  it should "merge by making a new one out of the old ones and offseting to keep the indents clean" in {
    IndentAndString.merge("new", IndentAndString(200, oneTwo), IndentAndString(2, threeFour)) shouldBe
      IndentAndString(201, List((200, "new"), (3, "one"), (4, "two"), (3, "three"), (4, "four")))
  }

}
