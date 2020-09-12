package one.xingyi.core.parserAndWriter

import one.xingyi.core.json.{FromJson, FromJsonLib, JsonParser}

trait Parser[T] {
  def apply(s: String): T
}

object Parser {
  implicit def default[J, T](implicit jsonParser: JsonParser[J], fromJsonLib: FromJsonLib[J, T]): FromJson[T] = s => fromJsonLib(jsonParser(s))

  implicit object ParserForString extends Parser[String] {def apply(s: String): String = s}
  implicit object ParserForDouble extends Parser[Double] {def apply(s: String): Double = s.toDouble}
  implicit object ParserForBoolean extends Parser[Boolean] {def apply(s: String): Boolean = s.toBoolean}
}