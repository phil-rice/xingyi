package one.xingyi.kwikServer

import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

trait KwikFixture {

  implicit def convertStringToIterator(s: String): Iterator[String] = Source.fromString(s).getLines()

  val title123 =
    """title 3
      |one 1
      |two 2
      |three 3""".stripMargin
  val tlv123 = TitleLengthValue("title", List("one 1", "two 2", "three 3"))
  val title123WithMoreStuff = title123 + "\nsomemorestuff"
}

class TitleLengthValueTest extends FlatSpec with Matchers with KwikFixture {

  behavior of TitleLengthValue.getClass.getSimpleName


  it should "parse from a string where the title and count of lines are the first line separated by a space" in {
    TitleLengthValue.parse(title123) shouldBe tlv123
    TitleLengthValue.parse(title123WithMoreStuff) shouldBe tlv123
  }

  it should "not read past the end of the title/value" in {
    val s: Iterator[String] = title123WithMoreStuff
    TitleLengthValue.parse(s) shouldBe tlv123
    s.next() shouldBe "somemorestuff"
  }

  it should "be creatable from a title and string for testing purposes" in {
    TitleLengthValue.fromStrings("title",
      """one 1
        |two 2
        |three 3""".stripMargin) shouldBe tlv123
  }

  it should "produce the toString used by the parser" in {
    TitleLengthValue.parse(tlv123.toString) shouldBe tlv123
  }

  it should "parse a list" in {
    TitleLengthValue.parseList(title123 + "\n" + title123) shouldBe List(tlv123, tlv123)
    }
}
