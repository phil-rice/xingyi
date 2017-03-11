package org.validoc.playJson

import org.validoc.utils.caching.CachableResultUsingSucesses
import org.validoc.utils.http.{Body, ContentType, ServiceResponse, Status}
import play.api.libs.json.{Json, OFormat}

import scala.language.implicitConversions
import scala.reflect.ClassTag

abstract class PlayJsonDomainObject[T: ClassTag : Manifest] {

  implicit val modelFormat: OFormat[T]
  // this will typically be implicit val modelFormat: OFormat[T]  = Json.format[T] but needs to be given with T hard coded

  def defaultContentType = ContentType("application/json")

  implicit def parserFinder = JsonParserFinder.allways[T]

  implicit object CachableResultForPromotion extends CachableResultUsingSucesses[T]

  implicit def toServiceResponse(t: T) = ServiceResponse(Status.Ok, Body(Json.stringify(Json.toJson[T](t))), defaultContentType)

  implicit def fromServiceResponse(serviceResponse: ServiceResponse): T =
    parserFinder.find(defaultContentType).valueOrException(serviceResponse.body.s)

}
