/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.json

import one.xingyi.core.UtilsSpec

import scala.reflect.ClassTag

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
}
