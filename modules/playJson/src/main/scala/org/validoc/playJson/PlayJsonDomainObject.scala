package org.validoc.playJson

import org.validoc.utils.caching.CachableResultUsingSucesses
import org.validoc.utils.http.{Body, ContentType, ServiceResponse, Status}
import play.api.libs.json.{Json, OFormat}

import scala.language.implicitConversions
import scala.reflect.ClassTag

trait PlayJsonDomainItem[T]

abstract class PlayJsonQueryKey[T: ClassTag : Manifest] extends PlayJsonDomainItem[T] {

}

abstract class PlayJsonDomainObject[T: ClassTag : Manifest] extends PlayJsonDomainItem[T] {

  implicit val modelFormat: OFormat[T]

  def defaultContentType = ContentType("application/json")

  implicit def parserFinder = JsonParserFinder.allways[T]

  implicit val cachableResult = new CachableResultUsingSucesses[T] {}

  /** used to display the object on the web page internal pages even if a 'middle object' and also used to return the 'final result' from the 'endpoint' objects */
  implicit def toServiceResponse(t: T) = ServiceResponse(Status.Ok, Body(Json.stringify(Json.toJson[T](t))), defaultContentType)

  implicit def fromServiceResponse(serviceResponse: ServiceResponse): T =
    parserFinder.find(defaultContentType).valueOrException(serviceResponse.body.s)

}
