package org.validoc.utils.domain

import org.validoc.utils.cache.ShouldUseCache
import org.validoc.utils.http._
import org.validoc.utils.parser.Parser

import scala.language.{higherKinds, reflectiveCalls}

//TODO This is a little messy: a hangover from a pervious iteration. Clean up this and 'should cache' and 'cachable'
trait BypassCache {
  def bypassCache: Boolean
}

trait AbstractDomain[T] {
  def defaultContentType = ContentType("application/json")
}

trait DomainRequestCompanionQuery[T <: BypassCache] extends AbstractDomain[T] {

  implicit def shouldCache = new ShouldUseCache[T] {
    override def apply(v1: T) = !v1.bypassCache
  }
}

trait DomainResponseCompanionObject[Req, Res] extends AbstractDomain[Res] {

  implicit def responseParser[M[_], Fail](implicit parser: Parser[Res]): ResponseParser[ Req, Res] = ResponseParser.defaultDirtyParser[M,  Req, Res]

}
