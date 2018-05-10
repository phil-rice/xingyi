package one.xingyi.core.http

import org.mockito.Mockito._
import one.xingyi.core._
import one.xingyi.core.parser.Parser

class ResponseProcessorTests extends UtilsWithLoggingSpec {
  val serviceResponse = ServiceResponse(Status(200), Body("someBody"), ContentType("someContentType"))
  val requestAndServiceResponse = RequestAndServiceResponse(5, serviceResponse)

  import Failer.failerForThrowable

  behavior of "default response parser"

  it should "use the implicit parser" in {
    implicit val parser = mock[Parser[String]]
    val responseParser = implicitly[ResponseParser[Int, String]]
    when(parser.apply("someBody")) thenReturn "someResult"
    responseParser.parse[Throwable](requestAndServiceResponse) shouldBe Right("someResult")
  }
}