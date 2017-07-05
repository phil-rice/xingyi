package org.validoc.utils.service

import org.validoc.utils.Service
import org.validoc.utils.concurrency.Async
import org.validoc.utils.http._
import org.validoc.utils.monads.CanMap._

import scala.language.higherKinds

trait OriginalReq[Req] {
  def acceptHeader(req: Req): AcceptHeader

  def header(req: Req, name: String): Header

  def contentType(req: Req): ContentType

  def method(req: Req): Method

  def path(req: Req): Path
}

trait EndPointOps[M[_]] extends Service[M, ServiceRequest, ServiceResponse] {
  def path: String
}

trait EndPointServiceLanguage[M[_]] extends ServiceComposition [M]{
  def endpoint[ Req: FromServiceRequest, Res: ToServiceResponse](path: String)(implicit async: Async[M]) =
    serviceWithParam[ String, Req, Res, ServiceRequest, ServiceResponse, EndPointService[M, Req, Res]](path,
      { (path: String, delegate: (Req) => M[Res]) => new EndPointService(path, delegate) })
}

class EndPointService[M[_] : Async, Req: FromServiceRequest, Res: ToServiceResponse](val path: String, delegate: Service[M, Req, Res]) extends EndPointOps[M] {
  val fromServiceRequest = implicitly[FromServiceRequest[Req]]
  val toServiceResponse = implicitly[ToServiceResponse[Res]]


  override def apply(serviceRequest: ServiceRequest): M[ServiceResponse] =
    (fromServiceRequest andThen delegate >>> toServiceResponse) (serviceRequest)

}

