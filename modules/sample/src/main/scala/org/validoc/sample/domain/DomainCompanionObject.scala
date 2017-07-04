package org.validoc.sample.domain

import org.validoc.utils.caching.CachableResultUsingSucesses
import org.validoc.utils.http._
import org.validoc.utils.json.ToJson
import org.validoc.utils.parser.ParserFinder


abstract class DomainCompanionObject[T] {
  def defaultContentType = ContentType("application/json")

  implicit val cachableResult = new CachableResultUsingSucesses[T] {}

  implicit def toServiceResponse(implicit toJson: ToJson[T]) = new ToServiceResponse[T] {
    override def apply(t: T): ServiceResponse = ServiceResponse(Status.Ok, Body(toJson(t)), defaultContentType)
  }

  implicit def fromServiceResponse(implicit parserFinder: ParserFinder[T]) = new FromServiceResponse[T] {
    override def apply(serviceResponse: ServiceResponse): T =
      parserFinder.find(defaultContentType).valueOrException(serviceResponse.body.s)
  }

}
