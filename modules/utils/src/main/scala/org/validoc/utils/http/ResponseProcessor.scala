package org.validoc.utils.http

import org.validoc.utils.GatewayException
import org.validoc.utils.logging.Logging

object ResponseProcessor {
  def parsed[Query, T, F](parser: String => T) = new ResponseProcessorExpectingResult[Query, T, F](parser)

  def optionalParsed[Query, T, F](parser: String => T) = new ResponseProcessorForOption[Query, T, F](parser)

  def parsedOrIssue[Query, T, Issue](parser: String => T, issuer: (Query) => Issue) = new ResponseProcessorForIssue[Query, T, Issue](parser, issuer)
}

case class RequestDetails[Req](req: Req, requestSummary: String)

trait ResponseProcessor[Req, T, F] {
  def statusOk(serviceResponse: ServiceResponse): T

  def statusNotFound(requestDetails: RequestDetails[Req], serviceResponse: ServiceResponse): T

  def statusUnexpected(requestDetails: RequestDetails[Req], serviceResponse: ServiceResponse): T = {
    throw GatewayException(requestDetails, serviceResponse)
  }

  def exception(e: F): F = e
}


class ResponseProcessorExpectingResult[Req, T, F](parser: String => T) extends ResponseProcessor[Req, T, F] {
  override def statusOk(serviceResponse: ServiceResponse): T = parser(serviceResponse.body.s)

  override def statusNotFound(requestDetails: RequestDetails[Req], serviceResponse: ServiceResponse): T =
    statusUnexpected(requestDetails, serviceResponse)
}

class ResponseProcessorForOption[Req, T, F](parser: String => T) extends ResponseProcessor[Req, Option[T],F] with Logging {
  override def statusOk(serviceResponse: ServiceResponse): Option[T] = Some(parser(serviceResponse.body.s))

  override def statusNotFound(requestDetails: RequestDetails[Req], serviceResponse: ServiceResponse): Option[T] = None
}

class ResponseProcessorForIssue[Req, T, Issue](parser: String => T, issuer: (Req) => Issue) extends ResponseProcessor[Req, Either[T, Issue], Issue] {
  override def statusOk(serviceResponse: ServiceResponse) = Left(parser(serviceResponse.body.s))

  override def statusNotFound(requestDetails: RequestDetails[Req], serviceResponse: ServiceResponse): Either[T, Issue] = Right(issuer(requestDetails.req))
}
