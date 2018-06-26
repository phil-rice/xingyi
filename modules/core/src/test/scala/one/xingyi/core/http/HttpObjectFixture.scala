/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.http

import one.xingyi.core.language.Language._
import one.xingyi.core.monad.Liftable

import scala.language.higherKinds

trait HttpObjectFixture {

  case class HttpRes(s: String)

  case class HttpReq(s: String)

  type Req = String
  type Res = String

  //    override def toSummary(req: Req): String = s"summary_$req"

  implicit object ToHttpRequestForReq extends ToServiceRequest[Req] {
    override def apply(req: Req): ServiceRequest = ServiceRequest(Get, Uri(req))
  }

  implicit def fromServiceRequestForHttpReq[M[_] : Liftable]: FromServiceRequest[M, HttpReq] = new FromServiceRequestForHttpReq[M]

  class FromServiceRequestForHttpReq[M[_] : Liftable] extends FromServiceRequest[M, HttpReq] {
    override def apply(s: ServiceRequest): M[HttpReq] = HttpReq(s.uri.asUriString).liftM
  }

}
