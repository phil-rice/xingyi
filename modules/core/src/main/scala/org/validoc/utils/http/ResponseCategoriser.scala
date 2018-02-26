package org.validoc.utils.http

import scala.annotation.implicitNotFound


trait ResponseCategoriser[Req] extends (Req => ServiceResponse => ResponseState[Req]) {}

sealed trait ResponseState[Req] {
  def req: Req
}
case class ResponseOk[Req](req: Req, serviceResponse: ServiceResponse) extends ResponseState[Req]
case class ResponseNotFound[Req](req: Req, serviceResponse: ServiceResponse) extends ResponseState[Req]
case class ResponseUnexpectedStatusCode[Req](req: Req, serviceResponse: ServiceResponse) extends ResponseState[Req]
case class ResponseException[Req](req: Req, e: Exception) extends ResponseState[Req]

object ResponseCategoriser {
  def apply[Req](): ResponseCategoriser[Req] = new ResponseCategoriser[Req] {
    override def apply(req: Req) = { serviceResponse =>
      serviceResponse.status.code match {
        case x if x / 100 == 2 => ResponseOk(req, serviceResponse)
        case 404 => ResponseNotFound(req, serviceResponse)
        case _ => ResponseUnexpectedStatusCode(req, serviceResponse)
      }
    }
  }

  implicit def default[Req] = apply[Req]()
}
