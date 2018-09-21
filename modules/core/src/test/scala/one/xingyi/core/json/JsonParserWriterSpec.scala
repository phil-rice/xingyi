/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.json

import one.xingyi.core.UtilsSpec
import one.xingyi.core.parser.Parser

import scala.reflect.ClassTag

case class ThingForJsonTests(a: Int, b: String, children: Seq[ChildForJsonTests])
case class ChildForJsonTests(c: Double, d: Boolean)

abstract class JsonParserWriterSpec[J: ClassTag](implicit val jsonParser: JsonParser[J], jsonWriter: JsonWriter[J]) extends UtilsSpec with JsonParserLanguage with JsonWriterLangauge {

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

  it should "turn a string into a 'J'" in {
    val i: Int = jsonParser("""{"a":1}""") \ "a"
    i shouldBe 1
  }


  it should "have a toJ method" in {
    val j: J = jsonWriter.toJ(JsonList(Seq(JsonObject("a" -> 1, "b" -> 1.0, "c" -> "1", "d" -> true))))
    jsonWriter.toStringForJ(j).noWhiteSpace shouldBe """[{"a":1,"b":1.0,"c":"1","d":true}]"""
  }




  it should "turn a J into a string" in {
    jsonWriter.toStringForJ(jsonParser("""{"a":1}""")).noWhiteSpace shouldBe """{"a":1}"""
  }


  behavior of "JsonObject"

  it should "allow extra fields to be added with |+|" in {
    JsonObject("a" -> 1, "b" -> 2) |+| "c" -> 3 shouldBe JsonObject("a" -> 1, "b" -> 2, "c" -> 3)
  }

  it should "have an adequate toString method" in {
    JsonObject("a" -> 1, "b" -> 2, "c" -> 3).toString shouldBe """JsonObject((a,JsonInt(1)),(b,JsonInt(2)),(c,JsonInt(3)))"""

  }
  behavior of "Parser"

  it should "turns a String into a T as long as the jsonparser exists" in {
    case class SimpleForTest(i: Int)
    implicit object fromJsonLib extends FromJsonLib[J, SimpleForTest] {
      override def apply(v1: J): SimpleForTest = SimpleForTest(v1 \ "a")
    }

    Parser.default[J, SimpleForTest].apply("""{"a":1}""") shouldBe SimpleForTest(1)
  }

  it should "support a map method for lists" in {
    val j: J = jsonWriter.toJ(JsonList(List(1, 2, 3)))
    new JsonParserOps(j).map(json => jsonParser.extractInt(json).toString) shouldBe List("1", "2", "3")
  }

  implicit def toJsonLibForChild: ToJsonLib[ChildForJsonTests] = t => JsonObject("c" -> t.c, "d" -> t.d)
  implicit def toJsonLibForThing: ToJsonLib[ThingForJsonTests] = t => JsonObject("a" -> t.a, "b" -> t.b, "children" -> toListT(t.children))
  implicit def fromJsonForChild: FromJsonLib[J, ChildForJsonTests] = json => ChildForJsonTests(json \ "c", json \ "d")
  implicit def fromJsonForThing: FromJsonLib[J, ThingForJsonTests] = json => ThingForJsonTests(json \ "a", json \ "b", (json \ "children").asList[ChildForJsonTests])

  behavior of "round trip objects"

  val child1 = ChildForJsonTests(1.0, true)
  val child2 = ChildForJsonTests(2.0, false)
  val thingy = ThingForJsonTests(1, "two", List(child1, child2))
  it should "allow a composite objects to be roundtripped" in {
    val string = implicitly[ToJson[ThingForJsonTests]] apply thingy
    val roundTrip = implicitly[FromJson[ThingForJsonTests]] apply string
    roundTrip shouldBe thingy
  }
  it should "Allow things with ToJsonLibs to be implicitly converted to json" in {
    val j: JsonValue = toT(thingy)
    j shouldBe JsonObject("a"->1, "b"->"two", "children"-> toListT(Seq(child1, child2)))
  }


}
