package org.validoc.utils.http

import org.validoc.utils.GatewayException
import org.validoc.utils.logging.Logging

object ResponseProcessor {
  def parsed[Query, T](parser: String => T) = new ResponseProcessorExpectingResult[Query, T](parser)

  def optionalParsed[Query, T](parser: String => T) = new ResponseProcessorForOption[Query, T](parser)

  def parsedOrIssue[Query, T, Issue](parser: String => T, issuer: (Query) => Issue) = new ResponseProcessorForIssue[Query, T, Issue](parser, issuer)
}

case class RequestDetails[Req](req: Req, requestSummary: String)

trait ResponseProcessor[Query, T] {
  def statusOk(serviceResponse: ServiceResponse): T

  def statusNotFound(requestDetails: RequestDetails[Query], serviceResponse: ServiceResponse): T

  def statusUnexpected(requestDetails: RequestDetails[Query], serviceResponse: ServiceResponse): T = {
    throw GatewayException(requestDetails, serviceResponse)
  }

  def exception(e: Throwable): T = throw e
}


class ResponseProcessorExpectingResult[Query, T](parser: String => T) extends ResponseProcessor[Query, T] {
  override def statusOk(serviceResponse: ServiceResponse): T = parser(serviceResponse.body.s)

  override def statusNotFound(requestDetails: RequestDetails[Query], serviceResponse: ServiceResponse): T =
    statusUnexpected(requestDetails, serviceResponse)
}

class ResponseProcessorForOption[Query, T](parser: String => T) extends ResponseProcessor[Query, Option[T]] with Logging {
  override def statusOk(serviceResponse: ServiceResponse): Option[T] = Some(parser(serviceResponse.body.s))

  override def statusNotFound(requestDetails: RequestDetails[Query], serviceResponse: ServiceResponse): Option[T] = None
}

class ResponseProcessorForIssue[Query, T, Issue](parser: String => T, issuer: (Query) => Issue) extends ResponseProcessor[Query, Either[T, Issue]] {
  override def statusOk(serviceResponse: ServiceResponse) = Left(parser(serviceResponse.body.s))

  override def statusNotFound(requestDetails: RequestDetails[Query], serviceResponse: ServiceResponse): Either[T, Issue] = Right(issuer(requestDetails.req))
}
