package one.xingyi.core.json

trait WriteToJson[T] {
  def apply(t: T): JsonValue
}

object WriteToJson {
  implicit def WriteToJsonForString: WriteToJson[String] = (t: String) => JsonString(t)
  implicit def WriteToJsonFoDouble: WriteToJson[Double] = (t: Double) => JsonDouble(t)
  implicit def WriteToJsonFoBoolean: WriteToJson[Boolean] = (t: Boolean) => JsonBoolean(t)
}