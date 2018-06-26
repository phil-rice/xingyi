/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.json

import scala.language.implicitConversions


trait ToJson[T] extends (T => String)

object ToJson {
  implicit def default[J, T](implicit jsonWriter: JsonWriter[J], toJsonLib: ToJsonLib[T]): ToJson[T] = t => jsonWriter(toJsonLib(t))
}


sealed trait JsonValue
case class JsonString(s: String) extends JsonValue
case class JsonInt(i: Int) extends JsonValue
case class JsonDouble(d: Double) extends JsonValue
case class JsonBoolean(b: Boolean) extends JsonValue
object JsonObject{
//  def from(name: String, value: JsonValue) = JsonObject(name -> value)
}
case class JsonObject(nameAndValues: (String, JsonValue)*) extends JsonValue {
  def |+|(other: (String, JsonValue)*) = JsonObject((nameAndValues ++ other): _*)
  override def toString: String = s"JsonObject(${nameAndValues.mkString(",")})"

}
case class JsonList(seq: Seq[JsonValue]) extends JsonValue

trait JsonWriter[J] extends (JsonValue => String) {
  def toJ(jsonValue: JsonValue): J
  def toStringForJ: J => String
  def apply(jsonValue: JsonValue) = toStringForJ(toJ(jsonValue))

}

trait ToJsonLib[T] extends (T => JsonValue)
trait JsonWriterLangauge {
  implicit def toJsonString(s: String) = JsonString(s)
  implicit def toJsonInt(i: Int) = JsonInt(i)
  implicit def toJsonBoolean(b: Boolean) = JsonBoolean(b)
  implicit def toJsonDouble(d: Double) = JsonDouble(d)
  implicit def toT[T](t: T)(implicit forT: ToJsonLib[T]): JsonValue = forT(t)
  implicit def toListT[T](ts: Seq[T])(implicit forT: ToJsonLib[T]): JsonList = JsonList(ts.map(forT))
}
