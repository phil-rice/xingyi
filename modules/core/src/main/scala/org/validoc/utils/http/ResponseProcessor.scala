package org.validoc.utils.http

import org.validoc.utils.exceptions.{EndpointNotFoundException, NotFoundException, ResponseParserException, UnexpectedStatusCodeException}
import org.validoc.utils.logging.DetailedLogging
import org.validoc.utils.parser.Parser

import scala.language.higherKinds

trait ResponseParser[Req, Res] {
  def parse[Fail](requestAndServiceResponse: RequestAndServiceResponse[Req])(implicit failer: ResponseParserFailer[Fail], reqDetails: DetailedLogging[Req], srDetails: DetailedLogging[ServiceResponse]): Either[Fail, Res]
}

object ResponseParser {
  implicit def defaultDirtyParser[M[_], Req: DetailedLogging, Res](implicit parser: Parser[Res], srDetails: DetailedLogging[ServiceResponse]) = new ResponseParser[Req, Res] {
    override def parse[Fail](requestAndServiceResponse: RequestAndServiceResponse[Req])(implicit failer: ResponseParserFailer[Fail], reqDetails: DetailedLogging[Req], srDetails: DetailedLogging[ServiceResponse]): Either[Fail, Res] =
      Right(parser(requestAndServiceResponse.serviceResponse.body.s))

  }
}

trait ResponseParserFailer[Fail] {
  def responseParserfailer[Req](requestAndServiceResponse: RequestAndServiceResponse[Req], info: String): Fail
}

trait Failer[Fail] extends ResponseParserFailer[Fail] {
  def notFound[Req](req: Req, response: ServiceResponse): Fail
  def unexpected[Req](req: Req, response: ServiceResponse): Fail
  def pathNotFound(serviceRequest: ServiceRequest): Fail
}

object Failer {


  implicit def failerForThrowable = new Failer[Throwable] {
    override def notFound[Req](req: Req, response: ServiceResponse) = new NotFoundException(req, response)
    override def unexpected[Req](req: Req, response: ServiceResponse) = new UnexpectedStatusCodeException(req, response)
    override def pathNotFound(serviceRequest: ServiceRequest) = new EndpointNotFoundException(serviceRequest)
    override def responseParserfailer[Req](requestAndServiceResponse: RequestAndServiceResponse[Req], info: String): Throwable =
      new ResponseParserException(requestAndServiceResponse.req, info, requestAndServiceResponse.serviceResponse)
  }

}

