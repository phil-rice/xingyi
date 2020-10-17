package one.xingyi.core.orm

import one.xingyi.core.json.{JsonString, WriteToJson}
import one.xingyi.core.parserAndWriter.{Parser, Writer}

class LinkUrlTest extends OrmKeyFixture {

  behavior of classOf[LinkUrl].getSimpleName

  val someUrlLink = LinkUrl("someUrl")
  implicit val zerothValueFromContext: ZerothValueFromContext[String] = s => s.take(1)

  val schemaWithlink: SchemaForTest[LinkUrl] = SchemaItem[LinkUrl]("some/{0}/with/{1}")

  it should "print and parse" in {
    implicitly[Parser[LinkUrl]].apply("someUrl") shouldBe someUrlLink
    implicitly[Writer[LinkUrl]].apply(someUrlLink) shouldBe "someUrl"
  }

  it should "have a writeto json " in {
    implicitly[WriteToJson[LinkUrl]].apply(someUrlLink) shouldBe JsonString("someUrl")
  }
  it should "be creatable with apply using the data in context and the schema" in {
    LinkUrl("xyz", schemaWithlink, List("zero", "one")) shouldBe LinkUrl("some/x/with/zero")
  }
}
