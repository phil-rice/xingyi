/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.http

import one.xingyi.core.UtilsSpec

import scala.concurrent.Future
import one.xingyi.core.monad.AsyncForScalaFuture._
import ImplicitsForTest._
import one.xingyi.core.json.ToJson
import org.mockito.Mockito._

class ServiceResponseSpec extends UtilsSpec {

  val serviceRequest = ServiceRequest(Method("get"), Uri("/someuri"))
  val serviceResponse = ServiceResponse(Status(200), Body("someBody"), ContentType("application/json"))

  "default from service response when have FromJson" should "use the tojson on the body" in {
    implicit val toJson = mock[ToJson[String]]
    val toServiceResponse = implicitly[ToServiceResponse[String, String]]
    when(toJson.apply("someString")) thenReturn "someBody"

    toServiceResponse("someIgnoredReq")("someString") shouldBe serviceResponse
  }

  behavior of "ServiceResponse"
  it should "let html responses be made easily" in {
    ServiceResponse("someHtml") shouldBe ServiceResponse(Status(200), Body("someHtml"), List(ContentType("text/html")))
  }

  it should "turn the content type into a header" in {
    ServiceResponse(Status(200), Body("someHtml"), ContentType("text/html")) shouldBe ServiceResponse(Status(200), Body("someHtml"), List(ContentType("text/html")))
  }
}
