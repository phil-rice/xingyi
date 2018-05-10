package one.xingyi.core.json

import one.xingyi.core.parser.Parser

import scala.language.implicitConversions
import one.xingyi.core.language.AnyLanguage._
trait FromJson[T] extends Parser[T]

object FromJson {
  implicit def default[J, T](implicit jsonParser: JsonParser[J], fromJsonLib: FromJsonLib[J, T]): FromJson[T] =
    s => fromJsonLib(jsonParser(s)).ifError { e => throw new RuntimeException(s"Error json parsing \n$s\n", e) }
}

trait JsonParser[J] extends (String => J) {
  def extractInt(j: J): Int
  def extractString(j: J): String
  def extractOptString(j: J): Option[String]
  def asList(j: J): List[J]
  def \(j: J, s: String): J
}

trait FromJsonLib[J, T] extends (J => T)

trait JsonParserLanguage {
  implicit def toString[J](j: J)(implicit parser: JsonParser[J]) = parser.extractString(j)
  implicit def toInt[J](j: J)(implicit parser: JsonParser[J]) = parser.extractInt(j)
  implicit def toOptString[J](j: J)(implicit parser: JsonParser[J]) = parser.extractOptString(j)

  implicit class JsonParserOps[J](j: J)(implicit jsonParser: JsonParser[J]) {
    def map[T1](fn: J => T1): List[T1] = jsonParser.asList(j).map(fn)
    def asList[T1](implicit fromJson: FromJsonLib[J, T1]): List[T1] = map[T1](fromJson)
    def as[T1](implicit fromJson: FromJsonLib[J, T1]): T1 = fromJson(j)
    def \(s: String): J = jsonParser.\(j, s)
  }

}
