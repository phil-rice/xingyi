/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.http

import one.xingyi.core.exceptions.{EndpointNotFoundException, NotFoundException, ResponseParserException, UnexpectedStatusCodeException}
import one.xingyi.core.logging.DetailedLogging
import one.xingyi.core.parser.Parser
import one.xingyi.core.script.IXingYi

import scala.language.higherKinds

trait ResponseParser[Req, Res] {
  def parse[Fail](requestAndServiceResponse: RequestAndServiceResponse[Req])(implicit failer: ResponseParserFailer[Fail], reqDetails: DetailedLogging[Req], srDetails: DetailedLogging[ServiceResponse]): Either[Fail, Res]
}

object ResponseParser {
  implicit def defaultDirtyParser[M[_], Req: DetailedLogging, Res](implicit parser: Parser[Res], srDetails: DetailedLogging[ServiceResponse]) = new ResponseParser[Req, Res] {
    override def parse[Fail](requestAndServiceResponse: RequestAndServiceResponse[Req])(implicit failer: ResponseParserFailer[Fail], reqDetails: DetailedLogging[Req], srDetails: DetailedLogging[ServiceResponse]): Either[Fail, Res] =
      Right(parser(requestAndServiceResponse.serviceResponse.body.s))

  }

  // so this will chance across time. Let's just 'prove it works' and then migrate to other stuff
 implicit def responseParserForXingyi[Req, Res](implicit xingYi: IXingYi, fromXingYi: FromXingYi[Req, Res]): ResponseParser[Req, Res] = new ResponseParser[Req, Res] {
    override def parse[Fail](requestAndServiceResponse: RequestAndServiceResponse[Req])(implicit failer: ResponseParserFailer[Fail], reqDetails: DetailedLogging[Req], srDetails: DetailedLogging[ServiceResponse]): Either[Fail, Res] = {
      import requestAndServiceResponse._
      serviceResponse.status.code / 100 match {
        case 2 => Right(fromXingYi(xingYi)(req)(serviceResponse.body.s))
        case _ => Left(failer.responseParserfailer(requestAndServiceResponse, "Unexpected status code "))
      }
    }
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

