package org.validoc.utils.http

import org.validoc.utils.containers.{Functor, Monad}
import org.validoc.utils.logging.Logging
import org.validoc.utils.parser.{FoundResult, ParserFinder, ParserResult}
import org.validoc.utils.{GatewayException, NotFoundException, UnexpectedException, UnexpectedParserException}


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
}


//trait ResponseProcessor[M[_], Req, T] {
//  def statusOk(req: Req, serviceResponse: ServiceResponse): M[T]
//
//  def statusNotFound(requestDetails: RequestDetails[Req], serviceResponse: ServiceResponse): M[T]
//
//  def statusUnexpected(requestDetails: RequestDetails[Req], serviceResponse: ServiceResponse): M[T] =
//    throw GatewayException(requestDetails, serviceResponse)
//
//  def exception(requestDetails: RequestDetails[Req])(t: Throwable): M[T] =
//    throw new UnexpectedException(requestDetails, t)
//
//  protected def process[T](parserFinder: ParserFinder[T], serviceResponse: ServiceResponse): ParserResult[T] =
//    try {
//      parserFinder(serviceResponse.contentType, serviceResponse.body.s)
//    } catch {
//      case t: Throwable => throw new UnexpectedParserException(serviceResponse, t)
//    }
//}
//
//
//class ResponseProcessorExpectingResult[M[_], Req, T](parserFinder: ParserFinder[T]) extends ResponseProcessor[M, Req, T] {
//  override def statusOk(serviceResponse: ServiceResponse) = process(parserFinder, serviceResponse).valueOrException
//
//  override def statusNotFound(requestDetails: RequestDetails[Req], serviceResponse: ServiceResponse) =
//    throw new NotFoundException(requestDetails, serviceResponse)
//}
//
//
//class ResponseProcessorForOption[Req, T](parserFinder: ParserFinder[T]) extends ResponseProcessor[Req, Option[T]] with Logging {
//  override def statusOk(serviceResponse: ServiceResponse) = process(parserFinder, serviceResponse).map(Some(_)).valueOrException
//
//  override def statusNotFound(requestDetails: RequestDetails[Req], serviceResponse: ServiceResponse) = FoundResult(serviceResponse.contentType, None).valueOrException
//}
//
//class ResponseProcessorForDefault[Req, T](parserFinder: ParserFinder[T], default: T) extends ResponseProcessor[Req, T] with Logging {
//  override def statusOk(serviceResponse: ServiceResponse) = process(parserFinder, serviceResponse).valueOrException
//
//  override def statusNotFound(requestDetails: RequestDetails[Req], serviceResponse: ServiceResponse) = default
//}
//
