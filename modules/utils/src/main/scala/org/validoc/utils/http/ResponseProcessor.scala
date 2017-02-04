package org.validoc.utils.http

import org.validoc.utils.GatewayException
import org.validoc.utils.logging.Logging

import scala.util.{Failure, Success, Try}

object ResponseProcessor {
  def parsed[Query, T](parser: String => T) = new ResponseProcessorExpectingResult[Query, T](parser)

  def optionalParsed[Query, T](parser: String => T) = new ResponseProcessorForOption[Query, T](parser)

}
case class RequestDetails[Req](req: Req, requestSummary: String)

trait ResponseProcessor[Req, T] {
  def statusOk(serviceResponse: ServiceResponse): T

  def statusNotFound(requestDetails: RequestDetails[Req], serviceResponse: ServiceResponse): T

  def statusUnexpected(requestDetails: RequestDetails[Req], serviceResponse: ServiceResponse): T = {
    throw GatewayException(requestDetails, serviceResponse)
  }

  def exception(t: Throwable): Try[T] = Failure(t)

}


class ResponseProcessorExpectingResult[Req, T](parser: String => T) extends ResponseProcessor[Req, T] {
  override def statusOk(serviceResponse: ServiceResponse): T = parser(serviceResponse.body.s)

  override def statusNotFound(requestDetails: RequestDetails[Req], serviceResponse: ServiceResponse): T =
    statusUnexpected(requestDetails, serviceResponse)
}

class ResponseProcessorForOption[Req, T](parser: String => T) extends ResponseProcessor[Req, Option[T]] with Logging {
  override def statusOk(serviceResponse: ServiceResponse): Option[T] = Some(parser(serviceResponse.body.s))

  override def statusNotFound(requestDetails: RequestDetails[Req], serviceResponse: ServiceResponse): Option[T] = None
}

