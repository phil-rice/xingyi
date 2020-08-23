package one.xingyi.core.databaseService

import one.xingyi.core.json.JsonLanguage.{toListT, _}
import one.xingyi.core.json._

object DatabaseServiceImplicits extends DatabaseServiceImplicits
trait DatabaseServiceImplicits {
  implicit def toJsonLibForResults[J: JsonWriter]: ToJsonLib[QueryResults] = results => JsonObject("names" -> toListT(results.title), "values" -> toListT(results.values))

}
