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
case class JsonObject(nameAndValues: (String, JsonValue)*) extends JsonValue
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
  implicit def toJsonDouble(d: Double) = JsonDouble(d)
  implicit def toT[T](t: T)(implicit forT: ToJsonLib[T]): JsonValue = forT(t)
  implicit def toListT[T](ts: Seq[T])(implicit forT: ToJsonLib[T]): JsonList = JsonList(ts.map(forT))
}
