package org.validoc.utils.endpoint

import org.validoc.utils.Service
import org.validoc.utils.concurrency.Async
import org.validoc.utils.http._

import scala.language.higherKinds

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


abstract class EndPointService[M[_] : Async, Req: FromServiceRequest, Res: ToServiceResponse](val path: String, delegate: Service[M, Req, Res])
  extends EndPointInfo[M] {

//  implicitly[FromServiceRequest[Req]] andThen implicitly[ToServiceResponse[Res]] >>> toServiceResponse

}

