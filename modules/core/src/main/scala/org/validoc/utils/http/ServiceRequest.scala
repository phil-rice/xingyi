package org.validoc.utils.http

import scala.annotation.implicitNotFound

case class ServiceRequest(method: Method, uri: Uri, acceptHeader: Option[AcceptHeader] = None, contentType: Option[ContentType] = None,  otherHeaders: List[Header] = List(), body: Option[Body] = None)
trait OriginalReq[Req] {
  def acceptHeader(req: Req): AcceptHeader
  def header(req: Req, name: String): Header
  def contentType(req: Req): ContentType
  def method(req: Req): Method
  def path(req: Req): Path
}

@implicitNotFound("""Missing ToServiceRequest[${T}] This is how we turn a query/request object (${T}) into a HTTP request. If ${T} is a http request have """)
trait ToServiceRequest[T] extends (T => ServiceRequest)

object ToServiceRequest {
  implicit object ToServiceRequestForServiceRequest extends ToServiceRequest[ServiceRequest] {
    override def apply(v1: ServiceRequest): ServiceRequest = v1
  }

}

@implicitNotFound("Missing FromServiceRequest[${T}]This is how we create a query/request (${T}) from an external clients HTTP request. It is isolated from exactly which webframework we are using.")
trait FromServiceRequest[T] extends (ServiceRequest => T)

object FromServiceRequest {
  implicit object FromServiceResponseForServiceResponse extends FromServiceRequest[ServiceRequest] {
    override def apply(v1: ServiceRequest): ServiceRequest = v1
  }
}