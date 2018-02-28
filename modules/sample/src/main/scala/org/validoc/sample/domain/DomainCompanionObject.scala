package org.validoc.sample.domain

import org.validoc.utils.cache.ShouldCache
import org.validoc.utils.http._
import org.validoc.utils.json.ToJson
import org.validoc.utils.parser.{Parser, ParserFinder}

import scala.language.reflectiveCalls

trait BypassCache {
  def bypassCache: Boolean
}
object BypassCache {
}

abstract class DomainCompanionQuery[T <: BypassCache] {
  def defaultContentType = ContentType("application/json")

  implicit def fromServiceResponse(implicit parserFinder: ParserFinder[T]) = new FromServiceResponse[T] {
    override def apply(serviceResponse: ServiceResponse): T =
      parserFinder.find(defaultContentType).valueOrException(serviceResponse.body.s)
  }
  implicit def shouldCache = new ShouldCache[T] {
    override def apply(v1: T) = !v1.bypassCache
  }
}

abstract class DomainCompanionObject[Req, Res] {
  def defaultContentType = ContentType("application/json")

  implicit def responseParser[Fail](implicit parser: Parser[Res]): ResponseParser[Fail, Req, Res] =
    ResponseParser.defaultDirtyParser[Fail, Req, Res]

}
