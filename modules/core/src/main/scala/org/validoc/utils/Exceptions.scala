package org.validoc.utils

import org.validoc.utils.http.{RequestDetails, ServiceResponse}
import org.validoc.utils.parser.{FailedParserResult, ParserResult}
import scala.language.existentials

case class GatewayException(requestDetails: RequestDetails[_], serviceResponse: ServiceResponse) extends
  Exception(s" RequestDetails $requestDetails\nResponse $serviceResponse")

case class NotFoundException(requestDetails: RequestDetails[_], serviceResponse: ServiceResponse) extends Exception(s"Status 404 for $requestDetails")


case class UnexpectedException(requestDetails: RequestDetails[_], t: Throwable) extends
  Exception(s" RequestDetails $requestDetails\nNested: $t", t)

class ParserException(val parserResult: ParserResult[_]) extends Exception(parserResult.toString)
class ParserNotFoundException(val parserResult: FailedParserResult[_]) extends Exception(parserResult.toString)

case class UnexpectedParserException(serviceResponse: ServiceResponse, t: Throwable) extends Exception(serviceResponse.toString, t)
