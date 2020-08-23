/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.json

import one.xingyi.core.UtilsSpec
import JsonLanguage._

trait JsonMapsFixture extends JsonLanguage {
  val objs = JsonObject("a" -> 1, "b" -> true, "c" -> JsonList(List("1", "2")), "d" -> JsonObject("e" -> 3.0d))

}
class JsonMapsSpec extends UtilsSpec with JsonMapsFixture {

  behavior of "JsonMaps"

  it should "turn a JsonValue into a map of stuff" in {
    JsonMaps.toMap(objs) shouldBe Map("a" -> 1, "b" -> true, "c" -> List("1", "2"), "d" -> Map("e" -> 3.0d))
  }
}
abstract class JsonMapsWithWriterSpec[J: JsonWriter] extends UtilsSpec with JsonMapsFixture {

  behavior of "JsonMaps"

  it should "make a JsonMaps object" in {
    val jsonMaps = JsonMaps(objs)
    jsonMaps.json.noWhiteSpace  shouldBe """{"a":1,"b":true,"c":["1","2"],"d":{"e":3.0}}"""
    jsonMaps.map shouldBe Map("a" -> 1, "b" -> true, "c" -> List("1", "2"), "d" -> Map("e" -> 3.0d))
  }

}
