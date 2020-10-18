package one.xingyi.core.json

import one.xingyi.core.orm.GetPattern

import scala.language.higherKinds

class GetJsonException(msg: String, exception: Exception) extends RuntimeException(msg, exception)

trait GetFromJson[T] {
  def apply[Schema[_] : GetPattern, J](s: Schema[T], j: J)(implicit jsonParser: JsonParser[J]): T
}

object GetFromJson {
  def wrap[J, T](j: J)(block: J => T): T = {
    try (block(j)) catch {case e: Exception => throw new GetJsonException(s"Trying to get from $j", e)}
  }
  def wrapAndTry[J, T](j: J)(block: J => T)(default: String => T)(implicit jsonParser: JsonParser[J]): T = {
    try (block(j)) catch {
      case e: Exception =>
        try {default(jsonParser.extractString(j)) } catch {
          case e2: Exception => throw new GetJsonException(s"Trying to get from $j", e)
        }
    }
  }
  implicit def getFromJsonForString: GetFromJson[String] = new GetFromJson[String] {
    override def apply[Schema[_] : GetPattern, J](s: Schema[String], j: J)(implicit jsonParser: JsonParser[J]): String = wrap(j)(jsonParser.extractString)
  }
  implicit def getFromJsonFoDouble: GetFromJson[Double] = new GetFromJson[Double] {
    override def apply[Schema[_] : GetPattern, J](s: Schema[Double], j: J)(implicit jsonParser: JsonParser[J]): Double = wrapAndTry(j)(jsonParser.extractDouble)(_.toDouble)
  }
  implicit def getFromJsonForInt: GetFromJson[Int] = new GetFromJson[Int] {
    override def apply[Schema[_] : GetPattern, J](s: Schema[Int], j: J)(implicit jsonParser: JsonParser[J]): Int = wrapAndTry(j)(jsonParser.extractDouble)(_.toDouble).toInt
  }
  implicit def getFromJsonFoBoolean: GetFromJson[Boolean] = new GetFromJson[Boolean] {
    override def apply[Schema[_] : GetPattern, J](s: Schema[Boolean], j: J)(implicit jsonParser: JsonParser[J]): Boolean = wrapAndTry(j)(jsonParser.extractBoolean)(_.toBoolean)
  }
}