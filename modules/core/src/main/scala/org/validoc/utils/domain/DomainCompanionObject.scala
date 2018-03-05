package org.validoc.utils.domain

import org.validoc.utils.cache.ShouldUseCache
import org.validoc.utils.http._
import org.validoc.utils.json.ToJson
import org.validoc.utils.parser.{Parser, ParserFinder}

import scala.language.reflectiveCalls
import scala.language.higherKinds
trait BypassCache {
  def bypassCache: Boolean
}
object BypassCache {
}

abstract class DomainCompanionQuery[T <: BypassCache] {
  def defaultContentType = ContentType("application/json")

//  implicit def fromServiceRequest(implicit parserFinder: ParserFinder[T]) = new FromServiceRequest[T] {
    //    override def apply(serviceResponse: ServiceResponse): T =
//    override def apply(serviceRequest: ServiceRequest) = parserFinder.find(defaultContentType).valueOrException(serviceRequest.body.s)
//  }
  implicit def shouldCache = new ShouldUseCache[T] {
    override def apply(v1: T) = !v1.bypassCache
  }
}

abstract class DomainCompanionObject[Req, Res] {
  def defaultContentType = ContentType("application/json")

  implicit def responseParser[M[_], Fail](implicit parser: Parser[Res]): ResponseParser[Fail, Req, Res] =
    ResponseParser.defaultDirtyParser[M, Fail, Req, Res]

}
