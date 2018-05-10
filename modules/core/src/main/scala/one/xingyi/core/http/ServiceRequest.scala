package one.xingyi.core.http

import one.xingyi.core.functions.{Liftable, Monad}
import one.xingyi.core.language.Language._
import scala.annotation.implicitNotFound
import scala.language.higherKinds

case class ServiceRequest(method: Method, uri: Uri, acceptHeader: Option[AcceptHeader] = None, contentType: Option[ContentType] = None, otherHeaders: List[Header] = List(), body: Option[Body] = None)

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
trait FromServiceRequest[M[_], T] extends (ServiceRequest => M[T])

object FromServiceRequest {
  implicit def FromServiceResponseForServiceResponse[M[_] : Liftable] = new FromServiceRequest[M, ServiceRequest] {
    override def apply(v1: ServiceRequest): M[ServiceRequest] = v1.liftM
  }
}