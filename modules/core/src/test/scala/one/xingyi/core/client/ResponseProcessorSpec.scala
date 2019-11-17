/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.client
import one.xingyi.core.UtilsSpec
import one.xingyi.core.http.{Body, ServiceRequest, ServiceResponse, Status}
import one.xingyi.core.json.FromJson
import one.xingyi.core.monad.IdentityMonad

import scala.util.Success

class ResponseProcessorSpec extends UtilsSpec {


  val rp = new AbstractResponseProcess[IdentityMonad, Int, String] {
    override def process200(req: Int, sr: ServiceResponse): IdentityMonad[String] = IdentityMonad(Success(sr.body.asUtf + "Processed"), Map())
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
