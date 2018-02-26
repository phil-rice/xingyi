package org.validoc.utils.http

import org.validoc.utils.json.ToJson
import org.validoc.utils.success._

import scala.annotation.implicitNotFound
import scala.util.{Failure, Success, Try}

case class ServiceResponse(status: Status, body: Body, contentType: ContentType)

object ServiceResponse {
  def fromJson[T](t: T)(implicit toJson: ToJson[T]) = ServiceResponse(Status(200), Body(toJson(t)), ContentType("application/json"))

}


@implicitNotFound("Missing ToServiceResponse[${T}] This turns ${T} into a service response so that it can be shown to the user. The simplest way to implement this is to have the domain object companion extend DomainCompanionObject and have a 'ToJson[${T}]' in the scope. This allows all decisions about which JSON library  we are using to be dealt with outside the main business logic")
trait ToServiceResponse[T] extends (T => ServiceResponse)

object ToServiceResponse {
  implicit object ToServiceResponseForServiceResponse extends ToServiceResponse[ServiceResponse] {
    override def apply(v1: ServiceResponse): ServiceResponse = v1
  }
}

@implicitNotFound("Missing FromServiceResponse[${T}] This creates a(${T}) from a service response returned by a client call. The simplest way to implement this is to have the domain object companion extend DomainCompanionObject and have a 'FromJson[${T}]' in the scope. This allows all decisions about which JSON library  we are using to be dealt with outside the main business logic")
trait FromServiceResponse[T] extends (ServiceResponse => T)

object FromServiceResponse {
  implicit object FromServiceResponseForServiceResponse extends FromServiceResponse[ServiceResponse] {
    override def apply(v1: ServiceResponse): ServiceResponse = v1
  }
}

trait EndpointPath[T] extends (ServiceResponse => Option[T])

