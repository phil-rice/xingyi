/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.json4s

import one.xingyi.core.json._
import org.json4s.JsonAST.{JArray, JObject}
import org.json4s.native.JsonMethods
import org.json4s.{DefaultFormats, JValue}

import scala.language.implicitConversions
import one.xingyi.core.language.FunctionLanguage._
import one.xingyi.core.language.AnyLanguage._

case class FromJson4sException(msg: String, cause: Throwable) extends Exception(msg, cause)

trait Json4sParser {

  implicit object JsonParserForJson4s extends JsonParser[JValue] {
    protected implicit val formats = DefaultFormats
    override def extractInt(j: JValue): Int = j.extract[Int]
    override def extractString(j: JValue): String = j.extract[String]
    override def extractOptString(j: JValue): Option[String] = j.extractOpt[String]
    override def asList(j: JValue): List[JValue] = j.extract[List[JValue]]
    override def \(j: JValue, s: String): JValue = j \ s
    override def apply(s: String): JValue = JsonMethods.parse(s).ifError(e => throw new FromJson4sException(s, e))
  }

}

object Json4sParser extends Json4sParser


trait Json4sWriter {

  implicit object JsonWriterForJson4s extends JsonWriter[JValue] {

    import org.json4s.JsonDSL._

    protected implicit val formats = DefaultFormats
    override def toJ(jsonValue: JsonValue): JValue = jsonValue match {
      case JsonString(s) => s
      case JsonInt(i) => i
      case JsonDouble(d) => d
      case JsonBoolean(b) => b
      case j: JsonObject => JObject(j.nameAndValues.map { case (k, v) => (k, toJ(v)) }: _*)
      case JsonList(list) => JArray(list.map(toJ).toList)
    }
    override def toStringForJ = JsonMethods.render _ ~> JsonMethods.pretty
  }

}

object Json4sWriter extends Json4sWriter

//trait FromJson4s[T] extends FromJson[T] {
//  protected implicit val formats = DefaultFormats
//  implicit def toString(j: JValue) = j.extract[String]
//  implicit def toOptString(j: JValue) = j.extractOpt[String]
//  implicit def toInt(j: JValue) = j.extract[Int]
//  override def apply(v1: String): T = parse(JsonMethods.parse(v1)).ifError(e => throw new FromJson4sException(s"Error parsing: $v1", e))
//  def parse(json: JValue): T
//
//  implicit class JValueOps(jValue: JValue) {
//    def asJList: List[JValue] = jValue.extract[List[JValue]]
//    def map[T1](fn: JValue => T1): List[T1] = asJList.map(fn)
//    def asList[T1](implicit fromJson4s: FromJson4s[T1]): List[T1] = map(fromJson4s.parse)
//    def as[T1](implicit fromJson4s: FromJson4s[T1]): T1 = fromJson4s.parse(jValue)
//  }
//
//}
//
//trait ToJson4s[T] extends ToJson[T] {
//  protected implicit val formats = DefaultFormats
//  def toJvalue(t: T): JValue
//  override def apply(v1: T): String = JsonMethods.pretty(JsonMethods.render(toJvalue(v1)))
//}
