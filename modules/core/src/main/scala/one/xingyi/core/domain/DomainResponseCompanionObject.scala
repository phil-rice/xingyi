/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.domain

import one.xingyi.core.cache.ShouldUseCache
import one.xingyi.core.http._
import one.xingyi.core.parser.Parser

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
