package one.xingyi.core.http

import one.xingyi.core.UtilsSpec

import scala.concurrent.Future
import one.xingyi.core.functions.AsyncForScalaFuture._
import ImplicitsForTest._
import one.xingyi.core.json.ToJson
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
