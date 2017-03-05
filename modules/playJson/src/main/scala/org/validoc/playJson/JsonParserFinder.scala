package org.validoc.playJson

import org.validoc.utils.parser.ParserFinder
import play.api.libs.json.{JsSuccess, Json, OFormat}

import scala.reflect.ClassTag

object JsonParserFinder {
  def allways[T: ClassTag : OFormat]: ParserFinder[T] = {
    val entityName = implicitly[ClassTag[T]].runtimeClass.getSimpleName
    ParserFinder.always(s =>
      Json.fromJson[T](Json.parse(s)) match {
        case JsSuccess(value, _) => value
      })

    //      .asJson.as[T].getOrException(MalformedJson(entityName, s)))
  }
}
