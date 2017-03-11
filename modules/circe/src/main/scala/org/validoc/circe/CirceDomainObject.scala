package org.validoc.circe

import io.circe.syntax._
import io.circe.{Decoder, Encoder}
import org.validoc.utils.caching.CachableResultUsingSucesses
import org.validoc.utils.http.{Body, ContentType, ServiceResponse, Status}

import scala.language.implicitConversions
import scala.reflect.ClassTag
abstract class CirceDomainObject[T: ClassTag : Manifest] {

  //Sadly this code
  //  implicit val encoder: Encoder[T] = deriveEncoder[T]
  //doesn't work in this class and has to be in the extending class
  implicit def encoder: Encoder[T]

  implicit def decoder: Decoder[T]

  def defaultContentType = ContentType("application/json")

  implicit def parserFinder = CirceParserFinder.allways[T]

  implicit object CachableResultForPromotion extends CachableResultUsingSucesses[T]

  implicit def toServiceResponse(t: T) = ServiceResponse(Status.Ok, Body(t.asJson.spaces2), defaultContentType)

}
