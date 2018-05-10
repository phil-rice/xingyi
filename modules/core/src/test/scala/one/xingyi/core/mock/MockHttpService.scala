package one.xingyi.core.mock

import one.xingyi.core.functions.Monad
import one.xingyi.core.http.{ServiceRequest, ServiceResponse, ToServiceRequest, ToServiceResponse}

import scala.language.higherKinds
import one.xingyi.core.language.Language._

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
