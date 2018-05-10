package one.xingyi.core.json

import one.xingyi.core.UtilsSpec

import scala.reflect.ClassTag

abstract class JsonParserWriterSpec[J: ClassTag](implicit jsonParser: JsonParser[J], jsonWriter: JsonWriter[J]) extends UtilsSpec with JsonParserLanguage with JsonWriterLangauge {

  def intAsJ(i: Int): J = jsonWriter.toJ(JsonInt(i))
  def stringAsJ(s: String): J = jsonWriter.toJ(JsonString(s))
  def doubleAsJ(d: Double): J = jsonWriter.toJ(JsonDouble(d))
  def listAsJ(list: List[Int]): J = jsonWriter.toJ(JsonList(list.map(JsonInt.apply)))

  // has {main: {a:1, b:2}, secondary: {c:3,d:4}}
  def mainA1B2SecondaryC3D4 = jsonWriter.toJ(JsonObject("main" -> JsonObject("a" -> 1, "b" -> "2"), "secondary" -> JsonObject("c" -> 3, "d" -> "4")))

  behavior of "JsonParser for " + implicitly[ClassTag[J]].runtimeClass.getName

  it should "extract ints" in {
    jsonParser.extractInt(intAsJ(1)) shouldBe 1
    jsonParser.extractInt(intAsJ(12)) shouldBe 12
    jsonParser.extractInt(intAsJ(-1)) shouldBe -1
  }

  it should "extract strings" in {
    jsonParser.extractString(stringAsJ("abc")) shouldBe "abc"
    jsonParser.extractString(stringAsJ("")) shouldBe ""
  }

  it should "extract options of strings " in {
    jsonParser.extractOptString(stringAsJ("abc")) shouldBe Some("abc")
    jsonParser.extractOptString(mainA1B2SecondaryC3D4 \ "a") shouldBe None

  }

  it should "change a J into a list of J if possible" in {
    jsonParser.asList(listAsJ(List(1, 2, 3))) shouldBe List(jsonWriter.toJ(JsonInt(1)), jsonWriter.toJ(JsonInt(2)), jsonWriter.toJ(JsonInt(3)))
  }

  it should "allow backslash and extraction" in {
    val a1: Int = mainA1B2SecondaryC3D4 \ "main" \ "a"
    a1 shouldBe 1
    val d4: String = mainA1B2SecondaryC3D4 \ "secondary" \ "d"
    d4 shouldBe "4"
  }

}
