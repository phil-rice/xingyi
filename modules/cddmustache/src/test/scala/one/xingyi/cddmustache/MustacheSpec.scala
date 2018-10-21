package one.xingyi.cddmustache
import one.xingyi.core.UtilsSpec
import one.xingyi.core.json.JsonWriterLangauge._
import one.xingyi.core.json.{JsonMaps, JsonObject, JsonWriter}

class AbstractMustacheSpec[J: JsonWriter] extends UtilsSpec {

  case class TestForMustache(one: String, two: Int)
  val jsonMap = JsonMaps(JsonObject("one" -> "111", "two" -> "map"))
  behavior of "RawMustache"

  it should "allow access to a template with a scala object" in {
    RawMustache("test.mustache")(TestForMustache("111", 222)) shouldBe "111 -> 222"
    //    jsonMap.asInstanceOf[]
    RawMustache("test.mustache")(jsonMap.map) shouldBe "111 -> map"
  }

  behavior of "MustacheMustache"

  it should "use the main template, passing the result of the template, the title and the json to it" in {
    implicit val mustacheTemplate = new Mustache("SomeTitle", "test.mustache", "mainTest.mustache")
    mustacheTemplate(jsonMap).noWhiteSpace shouldBe """Title:SomeTitle;Body:111->map;Json:{"one":"111","two":"map"}"""
  }

}
