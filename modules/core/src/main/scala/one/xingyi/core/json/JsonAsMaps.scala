/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.json

object JsonMaps {

  def apply[J: JsonWriter](jsonObject: JsonObject) = new JsonMaps[J](jsonObject)

  def toMap(j: JsonValue): Any = j match {
    case JsonString(s) => s
    case JsonInt(i) => i
    case JsonDouble(d) => d
    case JsonBoolean(b) => b
    case j: JsonObject => j.nameAndValues.map { case (k, v) => (k, toMap(v)) }.toMap
    case JsonList(list) => list.map(toMap)
  }

}
trait TemplateEngine[J] extends (JsonMaps[J] => String)

class JsonMaps[J](val jsonValue: JsonValue)(implicit jWriter: JsonWriter[J]) {
  lazy val map = JsonMaps.toMap(jsonValue)
  lazy val json = jWriter(jsonValue)
  override def toString: String = s"JsonMaps(${map})"
}
