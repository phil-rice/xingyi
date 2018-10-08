package one.xingyi.core.client
import one.xingyi.core.UtilsSpec
import one.xingyi.core.http.{Body, ServiceRequest, ServiceResponse, Status}
import one.xingyi.core.json.FromJson
import one.xingyi.core.monad.IdentityMonad

import scala.util.Success

class ResponseProcessorSpec extends UtilsSpec {


  val rp = new AbstractResponseProcess[IdentityMonad, Int, String] {
    override def process200(req: Int, sr: ServiceResponse): IdentityMonad[String] = IdentityMonad(Success(sr.body.s + "Processed"), Map())
  }

  behavior of "AbstractResponseProcessor"

  it should "call the process other if a 2xx is available" in {
    rp(1)(ServiceResponse(Status(200), Body("someBody"), List())).value shouldBe Success("someBodyProcessed")
    rp(1)(ServiceResponse(Status(299), Body("someBody"), List())).value shouldBe Success("someBodyProcessed")
  }

  it should "throw UnexpectedResponse if the status code isn't 2xx " in {
    intercept[UnexpectedResponse](rp(1)(ServiceResponse(Status(300), Body("someBody"), List())).value.get)
    intercept[UnexpectedResponse](rp(1)(ServiceResponse(Status(399), Body("someBody"), List())).value.get)
    intercept[UnexpectedResponse](rp(1)(ServiceResponse(Status(400), Body("someBody"), List())).value.get)
    intercept[UnexpectedResponse](rp(1)(ServiceResponse(Status(499), Body("someBody"), List())).value.get)
    intercept[UnexpectedResponse](rp(1)(ServiceResponse(Status(500), Body("someBody"), List())).value.get)
  }

  behavior of "DefaultResponseProcessor"

  it should "have a process200 method that just calls the toJson" in {
    implicit val fromJson: FromJson[String] = { json => json + "_processed" }
    val rp = ResponseProcessor.default[IdentityMonad, Int, String]
    rp(1)(ServiceResponse(Status(200), Body("someBody"), List())).value shouldBe Success("someBody_processed")
    rp(1)(ServiceResponse(Status(299), Body("someBody"), List())).value shouldBe Success("someBody_processed")
  }


}
