package one.xingyi.kwikServer

import org.scalatest.{FlatSpec, Matchers}

class VariableTest extends FlatSpec with Matchers with KwikFixture {

  behavior of Variable.getClass.getSimpleName

  it should "parse from a string with an expected title and not read over the end" in {
    val s: Iterator[String] =
      """title 3
        |one 1
        |two 2
        |three 3
        |otherstuff
        |""".stripMargin

    Variable.parseList("title")(s) shouldBe List(
      Variable("one", "1"),
      Variable("two", "2"),
      Variable("three", "3"))

    s.next() shouldBe "otherstuff"
  }

}
