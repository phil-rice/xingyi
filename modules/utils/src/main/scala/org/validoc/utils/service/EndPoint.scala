package org.validoc.utils.service

import org.validoc.utils.Service
import org.validoc.utils.concurrency.Async
import org.validoc.utils.http._
import org.validoc.utils.monads.CanMap._

trait OriginalReq[Req] {
  def acceptHeader(req: Req): AcceptHeader

  def header(req: Req, name: String): Header

  def contentType(req: Req): ContentType

  def method(req: Req): Method

  def path(req: Req): Path
}

class EndPointService[M[_]:Async, Req: FromServiceRequest, Res: ToServiceResponse](delegate: Service[M, Req, Res]) extends Service[M, ServiceRequest, ServiceResponse] {
  val fromServiceRequest = implicitly[FromServiceRequest[Req]]
  val toServiceResponse = implicitly[ToServiceResponse[Res]]


  override def apply(serviceRequest: ServiceRequest): M[ServiceResponse] =
   (fromServiceRequest andThen delegate >>> toServiceResponse) (serviceRequest)

}