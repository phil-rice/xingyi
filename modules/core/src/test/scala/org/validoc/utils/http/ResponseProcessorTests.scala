package org.validoc.utils.http

import org.validoc.utils.parser.ParserFinder
import org.validoc.utils.{NotFoundException, _}


class ResponseProcessorTests extends UtilsWithLoggingSpec {

  case class Result(s: String)

  val parserFinder = ParserFinder.always(Result(_))
  val runtimeException = new RuntimeException
  val errorThrowingParserFinder = ParserFinder.always[Result](s => throw runtimeException)

  val serviceResponse = ServiceResponse(Status(111), Body("someBody"), ContentType("some/content"))

  val requestDetails = RequestDetails("someReq", "someSummary")


  behavior of "ResponseProcessorExpectingResult"

  it should "have statusOk that returns a result" in {
    ResponseProcessor.parsed(parserFinder).statusOk(serviceResponse) shouldBe Result("someBody")
  }

  it should "nest any exception in the parser with a parser exception" in {
    intercept[UnexpectedParserException](ResponseProcessor.parsed(errorThrowingParserFinder).statusOk(serviceResponse)) shouldBe UnexpectedParserException(serviceResponse, runtimeException)
  }

  it should "have statusNotFound that throws a NotFoundException" in {
    intercept[NotFoundException](ResponseProcessor.parsed(parserFinder).statusNotFound(requestDetails, serviceResponse)) shouldBe
      NotFoundException(requestDetails, serviceResponse)
  }
  it should "have statusUnexpected that throws a GatewayException" in {
    intercept[GatewayException](ResponseProcessor.parsed(parserFinder).statusUnexpected(requestDetails, serviceResponse)) shouldBe
      GatewayException(requestDetails, serviceResponse)
  }

  it should "have a exception method that throws a unexpected exception" in {
    intercept[UnexpectedException](ResponseProcessor.parsed(parserFinder).exception(requestDetails)(runtimeException)) shouldBe UnexpectedException(requestDetails, runtimeException)
  }
  behavior of "ResponseProcessorForOption"

  it should "have statusOk that returns a result" in {
    ResponseProcessor.optionalParsed(parserFinder).statusOk(serviceResponse) shouldBe Some(Result("someBody"))
  }
  it should "nest any exception in the parser with a parser exception" in {
    intercept[UnexpectedParserException](ResponseProcessor.optionalParsed(errorThrowingParserFinder).statusOk(serviceResponse)) shouldBe UnexpectedParserException(serviceResponse, runtimeException)
  }

  it should "have statusNotFound that return None" in {
    ResponseProcessor.optionalParsed(parserFinder).statusNotFound(requestDetails, serviceResponse) shouldBe None
  }
  it should "have statusUnexpected that throws a GatewayException" in {
    intercept[GatewayException](ResponseProcessor.optionalParsed(parserFinder).statusUnexpected(requestDetails, serviceResponse)) shouldBe
      GatewayException(requestDetails, serviceResponse)
  }

  it should "have a exception method that returns a failure" in {
    intercept[UnexpectedException](ResponseProcessor.optionalParsed(parserFinder).exception(requestDetails)(runtimeException)) shouldBe UnexpectedException(requestDetails, runtimeException)
  }
}