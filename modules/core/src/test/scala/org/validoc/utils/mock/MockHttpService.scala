package org.validoc.utils.mock

import org.validoc.utils.concurrency.Async
import org.validoc.utils.concurrency.Async._
import org.validoc.utils.http.{ServiceRequest, ServiceResponse}
import org.validoc.utils.{FromServiceRequest, ToServiceResponse}
import scala.language.higherKinds
class MockHttpService[M[_] : Async, Req, Res](pf: PartialFunction[Req, M[Res]])
                                             (implicit fromServiceRequest: FromServiceRequest[Req],
                                              toServiceResponse: ToServiceResponse[Res]) extends PartialFunction[ServiceRequest, M[ServiceResponse]] {

  override def isDefinedAt(x: ServiceRequest): Boolean = pf.isDefinedAt(fromServiceRequest(x))

  override def apply(sr: ServiceRequest): M[ServiceResponse] = pf(fromServiceRequest(sr)).map(toServiceResponse)
}

object MockHttpService {
  def apply[M[_] : Async, Req: FromServiceRequest, Res: ToServiceResponse](pfs: PartialFunction[Req, M[Res]]*): PartialFunction[ServiceRequest, M[ServiceResponse]] =
    pfs.map(new MockHttpService[M, Req, Res](_)) reduce[PartialFunction[ServiceRequest, M[ServiceResponse]]] { case (acc, pf) => acc orElse pf }

}

