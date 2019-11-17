package one.xingyi.core.http
import org.scalatest.{FlatSpec, Matchers}

class BodyTest extends FlatSpec with Matchers {

  behavior of classOf[Body].getSimpleName

  val b123Nonea = new Body(Array(1, 2, 3), None)
  val b123Noneb = new Body(Array(1, 2, 3), None)
  val b456None = new Body(Array(4, 5, 6), None)
  val bodyOnea = Body("one")
  val bodyOneb = Body("one")
  val bodyTwo = Body("two")
  it should "define hashCode as the hashcode of the bytes" in {
    b123Nonea.hashCode() shouldBe b123Nonea.bytes.hashCode()
  }
  it should "define equals" in {
    b123Nonea should be(b123Noneb)

    b123Nonea shouldNot be(b456None)
    b123Nonea shouldNot be(null)

    bodyOnea shouldBe bodyOneb
    bodyOnea shouldNot be(bodyTwo)
    bodyTwo shouldBe bodyTwo
  }

  it should "be creatable from a string" in {
     bodyOnea shouldBe new Body("one".getBytes("utf-8"), Some("one"))
     b123Nonea shouldBe new Body(Array(1,2,3), None)
  }

  it should "have a asString that uses the string if it exists otherwise the default base64codec" in {
    bodyOnea.asString shouldBe "one"
    b123Nonea.asString shouldBe "AQID"
  }

  it should "have a toString that if a string shows the string" in {
    bodyOnea.toString shouldBe "Body(one)"
    bodyTwo.toString shouldBe "Body(two)"
  }

  it should "have a toString that marks up if it is a raw set of bytes" in {
    b123Nonea.toString shouldBe "Body<1,2,3>"
  }

}
