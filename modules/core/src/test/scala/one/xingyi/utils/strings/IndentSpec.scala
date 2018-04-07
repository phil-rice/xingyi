package one.xingyi.utils.strings

import one.xingyi.utils.{MonoidSpec, UtilsSpec}

class IndentSpec extends UtilsSpec {

  behavior of "IndentAndString"

  val oneTwo = List((1, "one"), (2, "two"))
  val threeFour = List((3, "three"), (4, "four"))
  it should "have a toString method with a marker" in {
     IndentAnd(3, oneTwo).defaultToString(",", "..." ) shouldBe "...one,......two"
    IndentAnd(3, List()).defaultToString(",", "...") shouldBe ""
  }

  it should "have a addLineAndIndent" in {
    IndentAnd(30, oneTwo).addLineAndIndent("new") shouldBe IndentAnd(31, oneTwo :+ ((30, "new")))
  }
  it should "have a insertLineAndIndent" in {
    IndentAnd(30, oneTwo).insertLineAndIndent("new") shouldBe IndentAnd(31, ((30, "new") :: oneTwo))
  }


  it should "have an unindent method that return the same lines but with depth reduced" in {
    IndentAnd(3, oneTwo).unindent shouldBe IndentAnd(2, oneTwo)
  }

  it should "invert the indent, which finds the biggest indent and makes that 0 . It leaes depth alone" in {
    IndentAnd(3, oneTwo).invertIndent shouldBe IndentAnd(3, List((1, "one"), (0, "two")))
  }

  it should "add offset to the indents " in {
    IndentAnd(3, oneTwo).offset(3) shouldBe IndentAnd(3, List((4, "one"), (5, "two")))
  }

  it should "merge by making a new one out of the old ones and offseting to keep the indents clean" in {
    IndentAnd.merge("new", IndentAnd(200, oneTwo), IndentAnd(2, threeFour)) shouldBe
      IndentAnd(201, List((200, "new"), (3, "one"), (4, "two"), (3, "three"), (4, "four")))
  }

}
