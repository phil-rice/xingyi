package one.xingyi.core.databaseService

import one.xingyi.core.json.JsonLanguage.{toListT, _}
import one.xingyi.core.json._

object DatabaseServiceImplicits extends DatabaseServiceImplicits
trait DatabaseServiceImplicits {
  implicit def fromJsonLibForMap[J](implicit parser: JsonParser[J]): FromJsonLib[J, Map[String, String]] = _.asObject match {
    case j: Map[String, J] => j.map { case (k, v) => k -> v.as[String] }.toMap[String, String]
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


  implicit def toJsonLibForMap[J]: ToJsonLib[Map[String, String]] = map =>
    JsonObject(map.map { case (k, v) => k -> JsonString(v) }.toList: _*)

  implicit def toJsonLibForString[J: JsonWriter]: ToJsonLib[String] = JsonString.apply
  implicit def toJsonLibForListString[J: JsonWriter]: ToJsonLib[List[String]] = toListT(_)
  implicit def toJsonLibForResults[J: JsonWriter]: ToJsonLib[QueryResults] = results => JsonObject("names" -> toListT(results.title), "values" -> toListT(results.values))

}
