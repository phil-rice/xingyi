/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.json

import one.xingyi.core.databaseService.{DatabaseRequest, QueryResults}
import one.xingyi.core.parserAndWriter.Parser

import scala.language.implicitConversions
import one.xingyi.core.language.AnyLanguage._

trait FromJson[T] extends Parser[T]

object FromJson {
}

trait JsonParser[J] extends (String => J) {
  def extractInt(j: J): Int
  def extractDouble(j: J): Double
  def extractBoolean(j: J): Boolean
  def extractString(j: J): String
  def extractOptString(j: J): Option[String]
  def asList(j: J): List[J]
  def asObject(j: J): Map[String, J]
  def \(j: J, s: String): J
}

trait  FromJsonLib[J, T] extends (J => T)

object FromJsonLib {
  implicit def fromString[J](implicit jsonParser: JsonParser[J]): FromJsonLib[J, String] = jsonParser.extractString(_)
  import JsonParserLanguage._

  implicit def fromJsonLibForMap[J](implicit parser: JsonParser[J]): FromJsonLib[J, Map[String, String]] = _.asObject match {
    case j: Map[String, J] => j.map { case (k, v) => k -> v.as[String] }
    case j => throw DatabaseRequest.needException(s"J was $j")
  }

  implicit def fromJsonLibForListT[J: JsonParser, T](implicit fromJsonLib: FromJsonLib[J, T]): FromJsonLib[J, List[T]] = {
    json => json.asList[T]
  }

  implicit def fromJsonLibForListString[J: JsonParser]: FromJsonLib[J, QueryResults] = { j =>
    val names = (j \ "names").asList[String]
    val values = (j \ "values").asList[List[String]]
    QueryResults(names, values)
  }

}
object JsonParserLanguage extends JsonParserLanguage
trait JsonParserLanguage {
  implicit def jsonToString[J](j: J)(implicit parser: JsonParser[J]) = parser.extractString(j)
  implicit def toInt[J](j: J)(implicit parser: JsonParser[J]) = parser.extractInt(j)
  implicit def toDouble[J](j: J)(implicit parser: JsonParser[J]) = parser.extractDouble(j)
  implicit def toBoolean[J](j: J)(implicit parser: JsonParser[J]) = parser.extractBoolean(j)
  implicit def toOptString[J](j: J)(implicit parser: JsonParser[J]) = parser.extractOptString(j)

  implicit class JsonParserOps[J](j: J)(implicit jsonParser: JsonParser[J]) {
    def map[T1](fn: J => T1): List[T1] = jsonParser.asList(j).map(fn)
    def asList[T1](implicit fromJson: FromJsonLib[J, T1]): List[T1] = map[T1](fromJson)
    def asObject: Map[String, J] = jsonParser.asObject(j)
    def asListP[Shared, Domain](implicit projection: Projection[Shared, Domain]): List[Domain] = map[Domain](projection.fromJson)
    def as[T1](implicit fromJson: FromJsonLib[J, T1]): T1 = fromJson(j)
    def \(s: String): J = jsonParser.\(j, s)
  }

}
