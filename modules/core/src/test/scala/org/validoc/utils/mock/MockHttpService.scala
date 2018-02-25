package org.validoc.utils.mock

import org.validoc.utils.functions.Monad
import org.validoc.utils.http.{ServiceRequest, ServiceResponse, ToServiceRequest, ToServiceResponse}

import scala.language.higherKinds

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
