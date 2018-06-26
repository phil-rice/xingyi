/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.mock
import one.xingyi.core.http.{ServiceRequest, ServiceResponse, ToServiceRequest, ToServiceResponse}

import scala.language.higherKinds
import one.xingyi.core.language.Language._
import one.xingyi.core.monad.Monad

class MockHttpService[M[_]:Monad, Req, Res](mocks: Map[Req, Res])
                                           (implicit toServiceRequest: ToServiceRequest[Req],
                                      toServiceResponse: ToServiceResponse[Res]) extends PartialFunction[ServiceRequest, M[ServiceResponse]] {
  val map = mocks.map { case (k, v) => (toServiceRequest(k), v) }


  override def apply(sr: ServiceRequest): M[ServiceResponse] = (map andThen toServiceResponse) (sr).liftM

  override def isDefinedAt(x: ServiceRequest): Boolean = ???
}

object MockHttpService {
  def apply[M[_] : Monad, Req: ToServiceRequest, Res: ToServiceResponse](tuples: (Req, Res)*): MockHttpService[M, Req, Res] =
    new MockHttpService[M, Req, Res](Map(tuples: _*))

  def apply[M[_]](services: MockHttpService[M, _, _]*) = services.reduce[PartialFunction[ServiceRequest, M[ServiceResponse]]]((acc, s) => acc orElse s)

}
