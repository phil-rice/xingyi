package org.validoc

import org.validoc.utils.http.{ServiceRequest, ServiceResponse}
import org.validoc.utils.service.ServerContext


package object utils {

  type Service[M[_], Req, Res] = (Req => M[Res])
  type Parser[T] = String => T

  type ToServiceResponse[T] = T => ServiceResponse
  type FromServiceResponse[T] = ServiceResponse => T

  type ToServiceRequest[T] = T => ServiceRequest
  type FromServiceRequest[T] = ServiceRequest => T

  implicit def toServiceRequestFrom[T](implicit serverContext: ServerContext[T, _]) = serverContext.toServiceRequest

  implicit def fromServiceRequestFrom[T](implicit serverContext: ServerContext[T, _]) = serverContext.fromServiceRequest

  implicit def toServiceResponseFrom[T](implicit serverContext: ServerContext[_, T]) = serverContext.toServiceResponse

}