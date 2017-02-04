package org.validoc.utils.http

import org.validoc.utils.{GatewayException, UtilsSpec}

import scala.util.Failure


class ResponseProcessorTests extends UtilsSpec {

  case class Result(s: String)

  def parser: String => Result = Result(_)

  val statusResponse = ServiceResponse(Status(111), Body("someBody"), ContentType("some/content"))

  val requestDetails = RequestDetails("someReq", "someSummary")

  val runtimeException = new RuntimeException

  behavior of "ResponseProcessorExpectingResult"

  it should "have statusOk that returns a result" in {
    ResponseProcessor.parsed(parser).statusOk(statusResponse) shouldBe Result("someBody")
  }

  it should "have statusNotFound that throws a GatewayException" in {
    intercept[GatewayException](ResponseProcessor.parsed(parser).statusNotFound(requestDetails, statusResponse)) shouldBe
      GatewayException(requestDetails, statusResponse)
  }
  it should "have statusUnexpected that throws a GatewayException" in {
    intercept[GatewayException](ResponseProcessor.parsed(parser).statusUnexpected(requestDetails, statusResponse)) shouldBe
      GatewayException(requestDetails, statusResponse)
  }

  it should "have a exception method that returns a failure" in {
    ResponseProcessor.parsed(parser).exception(runtimeException) shouldBe Failure(runtimeException)
  }
  behavior of "ResponseProcessorForOption"

  it should "have statusOk that returns a result" in {
    ResponseProcessor.optionalParsed(parser).statusOk(statusResponse) shouldBe Some(Result("someBody"))
  }

  it should "have statusNotFound that return None" in {
    ResponseProcessor.optionalParsed(parser).statusNotFound(requestDetails, statusResponse) shouldBe None
  }
  it should "have statusUnexpected that throws a GatewayException" in {
    intercept[GatewayException](ResponseProcessor.optionalParsed(parser).statusUnexpected(requestDetails, statusResponse)) shouldBe
      GatewayException(requestDetails, statusResponse)
  }

  it should "have a exception method that returns a failure" in {
    ResponseProcessor.optionalParsed(parser).exception(runtimeException) shouldBe Failure(runtimeException)
  }
}