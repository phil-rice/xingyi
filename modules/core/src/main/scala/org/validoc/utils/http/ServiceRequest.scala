package org.validoc.utils.http

import scala.annotation.implicitNotFound

case class ServiceRequest(method: Method, uri: Uri, acceptHeader: Option[AcceptHeader] = None, otherHeaders: List[Header] = List(), body: Option[Body] = None)

@implicitNotFound("Missing ToServiceRequest[${T}] This is how we turn a query/request object (${T}) into a HTTP request. It is isolated from exactly which webframework we are using.")
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