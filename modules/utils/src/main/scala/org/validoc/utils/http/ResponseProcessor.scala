package org.validoc.utils.http

import org.validoc.utils.{GatewayException, UnexpectedException, UnexpectedParserException}
import org.validoc.utils.logging.Logging
import org.validoc.utils.parser.{ErrorResult, FoundResult, ParserFinder, ParserResult}

object ResponseProcessor {
  def parsed[Query, T](parserFinder: ParserFinder[T]) = new ResponseProcessorExpectingResult[Query, T](parserFinder)

  def optionalParsed[Query, T](parserFinder: ParserFinder[T]) = new ResponseProcessorForOption[Query, T](parserFinder)
}

case class RequestDetails[Req](req: Req, requestSummary: String)

trait ResponseProcessor[Req, T] {
  def statusOk(serviceResponse: ServiceResponse): T

  def statusNotFound(requestDetails: RequestDetails[Req], serviceResponse: ServiceResponse): T

  def statusUnexpected(requestDetails: RequestDetails[Req], serviceResponse: ServiceResponse): T =
    throw GatewayException(requestDetails, serviceResponse)


  def exception(requestDetails: RequestDetails[Req], t: Throwable): T = throw new UnexpectedException(requestDetails, t)

  protected def process[T](parserFinder: ParserFinder[T], serviceResponse: ServiceResponse) =
    try {
      parserFinder(serviceResponse.contentType, serviceResponse.body.s)
    } catch {
      case t: Throwable => throw new UnexpectedParserException(serviceResponse, t)
    }
}


class ResponseProcessorExpectingResult[Req, T](parserFinder: ParserFinder[T]) extends ResponseProcessor[Req, T] {
  override def statusOk(serviceResponse: ServiceResponse) = process(parserFinder, serviceResponse).valueOrException

  override def statusNotFound(requestDetails: RequestDetails[Req], serviceResponse: ServiceResponse) =
    statusUnexpected(requestDetails, serviceResponse)
}

class ResponseProcessorForOption[Req, T](parserFinder: ParserFinder[T]) extends ResponseProcessor[Req, Option[T]] with Logging {
  override def statusOk(serviceResponse: ServiceResponse) = process(parserFinder, serviceResponse).map(Some(_)).valueOrException

  override def statusNotFound(requestDetails: RequestDetails[Req], serviceResponse: ServiceResponse) = FoundResult(serviceResponse.contentType, None).valueOrException
}

