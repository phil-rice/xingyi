package one.xingyi.core.json

class GetJsonException(msg: String, exception: Exception) extends RuntimeException(msg, exception)

trait GetFromJson[T] {
  def apply[J](j: J)(implicit jsonParser: JsonParser[J]): T
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
    override def apply[J](j: J)(implicit jsonParser: JsonParser[J]): String = wrap(j)(jsonParser.extractString)
  }
  implicit def getFromJsonFoDouble: GetFromJson[Double] = new GetFromJson[Double] {
    override def apply[J](j: J)(implicit jsonParser: JsonParser[J]): Double = wrapAndTry(j)(jsonParser.extractDouble)(_.toDouble)
  }
  implicit def getFromJsonFoBoolean: GetFromJson[Boolean] = new GetFromJson[Boolean] {
    override def apply[J](j: J)(implicit jsonParser: JsonParser[J]): Boolean = wrapAndTry(j)(jsonParser.extractBoolean)(_.toBoolean)
  }
}