/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.json

import one.xingyi.core.json.JsonLanguage.toListT

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
object JsonObject {
  //  def from(name: String, value: JsonValue) = JsonObject(name -> value)
}
case class JsonObject(nameAndValues: (String, JsonValue)*) extends JsonValue {
  def |+|(other: (String, JsonValue)*): JsonObject = add(other)
  def optionalAdd(name: String, value: Option[JsonValue]): JsonObject = value.fold(this)(v => JsonObject((nameAndValues :+ (name -> v)): _*))
  def addIf(b: Boolean, name: String, value: JsonValue): JsonObject = if (b) JsonObject((nameAndValues :+ (name -> value)): _*) else this
  def add(other: Seq[(String, JsonValue)]): JsonObject = JsonObject((nameAndValues ++ other): _*)
  def add(other: JsonObject): JsonObject = JsonObject((nameAndValues ++ other.nameAndValues): _*)
  override def toString: String = s"JsonObject(${nameAndValues.mkString(",")})"

}
case class JsonList(seq: Seq[JsonValue]) extends JsonValue

trait JsonWriter[J] extends (JsonValue => String) {
  def toJ(jsonValue: JsonValue): J
  def toStringForJ: J => String
  def apply(jsonValue: JsonValue): String = toStringForJ(toJ(jsonValue))


}

trait ToJsonLib[T] extends (T => JsonValue)

object ToJsonLib {
  implicit object toJsonLibForJsonObject extends ToJsonLib[JsonObject] {
    override def apply(v1: JsonObject): JsonValue = v1
  }
  implicit def toJsonLibForMap[J]: ToJsonLib[Map[String, String]] = map => JsonObject(map.map { case (k, v) => k -> JsonString(v) }.toList: _*)

  implicit def toJsonLibForString[J: JsonWriter]: ToJsonLib[String] = JsonString.apply
  implicit def toJsonLibForListString[J: JsonWriter]: ToJsonLib[List[String]] = toListT(_)
  implicit def toJsonLibForBoolean[J: JsonWriter]: ToJsonLib[Boolean] = JsonBoolean(_)
  implicit def toJsonLibForDouble[J: JsonWriter]: ToJsonLib[Double] = JsonDouble(_)
  implicit def toJsonLibForJsonValue[J: JsonWriter]: ToJsonLib[JsonValue] = x => x

}

object JsonWriterLanguage extends JsonWriterLanguage
trait JsonWriterLanguage {
  implicit def toJsonString(s: String): JsonString = JsonString(s)
  implicit def toJsonInt(i: Int): JsonInt = JsonInt(i)
  implicit def toJsonBoolean(b: Boolean): JsonBoolean = JsonBoolean(b)
  implicit def toJsonDouble(d: Double): JsonDouble = JsonDouble(d)

  implicit def toT[T](t: T)(implicit forT: ToJsonLib[T]): JsonValue = forT(t)
  implicit def toListT[T](ts: Seq[T])(implicit forT: ToJsonLib[T]): JsonList = JsonList(ts.map(forT))
  implicit object ToJsonLibForJsonValue extends ToJsonLib[JsonValue] {
    override def apply(jsonValue: JsonValue): JsonValue = jsonValue
  }

  implicit class jsonWriterOps[T](t: T)(implicit toJsonLib: ToJsonLib[T]) {
    def toJsonValue: JsonValue = toJsonLib(t)
    def toJ[J](implicit jsonWriter: JsonWriter[J]): J = jsonWriter.toJ(toJsonLib(t))
    def toJsonString[J](implicit jsonWriter: JsonWriter[J]): String = jsonWriter(toJsonValue)
  }

}
