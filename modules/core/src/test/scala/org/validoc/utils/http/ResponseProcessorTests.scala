package org.validoc.utils.http

import org.validoc.utils._
import org.validoc.utils.parser.ParserFinder


class ResponseProcessorTests extends UtilsWithLoggingSpec {

  case class Result(s: String)

//  val parserFinder = ParserFinder.always(Result(_))
//  val runtimeException = new RuntimeException
//  val errorThrowingParserFinder = ParserFinder.always[Result](s => throw runtimeException)
//
//  val serviceResponse = ServiceResponse(Status(111), Body("someBody"), ContentType("some/content"))

//  val requestDetails = RequestDetails("someReq", "someSummary")


  behavior of "ResponseProcessorExpectingResult"

  it should "be written" in {

  }
//  it should "have statusOk that returns a result" in {
//    ResponseCategoriser.parsed(parserFinder).statusOk(serviceResponse) shouldBe Result("someBody")
//  }
//
//  it should "nest any exception in the parser with a parser exception" in {
//    intercept[UnexpectedParserException](ResponseCategoriser.parsed(errorThrowingParserFinder).statusOk(serviceResponse)) shouldBe UnexpectedParserException(serviceResponse, runtimeException)
//  }
//
//  it should "have statusNotFound that throws a NotFoundException" in {
//    intercept[NotFoundException](ResponseCategoriser.parsed(parserFinder).statusNotFound(requestDetails, serviceResponse)) shouldBe
//      NotFoundException(requestDetails, serviceResponse)
//  }
//  it should "have statusUnexpected that throws a GatewayException" in {
//    intercept[GatewayException](ResponseCategoriser.parsed(parserFinder).statusUnexpected(requestDetails, serviceResponse)) shouldBe
//      GatewayException(requestDetails, serviceResponse)
//  }
//
//  it should "have a exception method that throws a unexpected exception" in {
//    intercept[UnexpectedException](ResponseCategoriser.parsed(parserFinder).exception(requestDetails)(runtimeException)) shouldBe UnexpectedException(requestDetails, runtimeException)
//  }
//  behavior of "ResponseProcessorForOption"
//
//  it should "have statusOk that returns a result" in {
//    ResponseCategoriser.optionalParsed(parserFinder).statusOk(serviceResponse) shouldBe Some(Result("someBody"))
//  }
//  it should "nest any exception in the parser with a parser exception" in {
//    intercept[UnexpectedParserException](ResponseCategoriser.optionalParsed(errorThrowingParserFinder).statusOk(serviceResponse)) shouldBe UnexpectedParserException(serviceResponse, runtimeException)
//  }
//
//  it should "have statusNotFound that return None" in {
//    ResponseCategoriser.optionalParsed(parserFinder).statusNotFound(requestDetails, serviceResponse) shouldBe None
//  }
//  it should "have statusUnexpected that throws a GatewayException" in {
//    intercept[GatewayException](ResponseCategoriser.optionalParsed(parserFinder).statusUnexpected(requestDetails, serviceResponse)) shouldBe
//      GatewayException(requestDetails, serviceResponse)
//  }
//
//  it should "have a exception method that returns a failure" in {
//    intercept[UnexpectedException](ResponseCategoriser.optionalParsed(parserFinder).exception(requestDetails)(runtimeException)) shouldBe UnexpectedException(requestDetails, runtimeException)
//  }
}