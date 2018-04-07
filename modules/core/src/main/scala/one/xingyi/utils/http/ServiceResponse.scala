package one.xingyi.utils.http

import one.xingyi.utils.json.ToJson
import one.xingyi.utils.profiling.{DontProfile, ProfileAs, ProfileAsFail, ProfileAsSuccess}
import one.xingyi.utils.success._

import scala.annotation.implicitNotFound
import scala.util.{Failure, Success, Try}

case class ServiceResponse(status: Status, body: Body, contentType: ContentType)



object ServiceResponse {
  def apply(html: String): ServiceResponse = ServiceResponse(Status(200), Body(html), ContentType("text/html"))
}

@implicitNotFound(
  """Missing ToServiceResponse[${T}] This turns ${T} into a service response so that it can be shown to the user. The simplest way to implement this is to have a 'ToJson[${T}]' in the scope.
    To debug this you can have the following code
    val x = implicitly[ToJson[${T}]]
    val y = implicitly[ToServiceResponse]

  """)
trait ToServiceResponse[T] extends (T => ServiceResponse)

object ToServiceResponse {

  implicit def toServiceResponse[T](implicit toJson: ToJson[T]) = new ToServiceResponse[T] {
    override def apply(t: T): ServiceResponse = ServiceResponse(Status.Ok, Body(toJson(t)), ContentType("application/json"))
  }

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

