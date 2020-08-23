/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.mustache
import one.xingyi.core.UtilsSpec
import one.xingyi.core.json.JsonWriterLanguage._
import one.xingyi.core.json.{JsonMaps, JsonObject, JsonWriter}

abstract class AbstractMustacheSpec[J: JsonWriter] extends UtilsSpec {

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
