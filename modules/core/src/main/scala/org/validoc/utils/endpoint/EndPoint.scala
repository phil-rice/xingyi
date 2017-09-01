package org.validoc.utils.endpoint

import org.validoc.utils.Service
import org.validoc.utils.concurrency.Async
import org.validoc.utils.http._
import org.validoc.utils.monads.CanMap._
import org.validoc.utils.serviceTree.ServiceLanguageExtension

import scala.language.higherKinds
import scala.reflect.ClassTag

trait OriginalReq[Req] {
  def acceptHeader(req: Req): AcceptHeader

  def header(req: Req, name: String): Header

  def contentType(req: Req): ContentType

  def method(req: Req): Method

  def path(req: Req): Path
}

trait EndPointInfo[M[_]] extends Service[M, ServiceRequest, ServiceResponse] {
  def path: String
}


class EndPointService[M[_] : Async, Req: FromServiceRequest, Res: ToServiceResponse](val path: String, delegate: Service[M, Req, Res]) extends EndPointInfo[M] {
  val fromServiceRequest = implicitly[FromServiceRequest[Req]]
  val toServiceResponse = implicitly[ToServiceResponse[Res]]


  override def apply(serviceRequest: ServiceRequest): M[ServiceResponse] =
    (fromServiceRequest andThen delegate >>> toServiceResponse) (serviceRequest)

}


trait EndPointServiceLanguageExtension[M[_]] extends ServiceLanguageExtension[M] {
  def endpoint[Req: ClassTag : FromServiceRequest, Res: ClassTag : ToServiceResponse](path: String): ServiceTransformer[Req, Res, ServiceRequest, ServiceResponse] = { childTree =>
    transform[Req, Res, ServiceRequest, ServiceResponse](s"Endpoint($path)", childTree, new EndPointService[M, Req, Res](path, _))
  }
}