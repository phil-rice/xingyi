package org.validoc.utils.http

import org.validoc.utils.UtilsSpec

import scala.concurrent.Future
import org.validoc.utils.functions.AsyncForScalaFuture._
import ImplicitsForTest._
import org.validoc.utils.json.ToJson
import org.mockito.Mockito._

class ServiceResponseSpec extends UtilsSpec {

  val serviceResponse = ServiceResponse(Status(200), Body("someBody"), ContentType("application/json"))

  "default from service response when have FromJson" should "use the tojson on the body" in {
    implicit val toJson = mock[ToJson[String]]
    val toServiceResponse = implicitly[ToServiceResponse[String]]
    when(toJson.apply("someString")) thenReturn "someBody"

    toServiceResponse("someString") shouldBe serviceResponse
  }
}
