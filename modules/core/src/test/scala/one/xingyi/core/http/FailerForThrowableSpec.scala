/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.http

import one.xingyi.core.UtilsSpec
import one.xingyi.core.exceptions.{EndpointNotFoundException, NotFoundException, UnexpectedStatusCodeException}
import one.xingyi.core.monad.ScalaFutureAsAsyncAndMonadAndFailer

class FailerForThrowableSpec extends UtilsSpec with ScalaFutureAsAsyncAndMonadAndFailer {


  val serviceRequest = ServiceRequest(Get, Uri("/someUri"))
  val serviceResponse = ServiceResponse(Status(200), Body("someBody"), ContentType("someContentType"))
  behavior of "FailerForThrowable"

  it should "Future.exception a NotFoundException when notFound" in {
    val m = failer.notFound("someReq", serviceResponse)
    val e = m.asInstanceOf[NotFoundException]
    e.response shouldBe serviceResponse
    e.req shouldBe "someReq"
  }

  it should "Future.exception a UnexpectedStatusCodeException when unexpected statusCode" in {
    val m = failer.unexpected("someReq", serviceResponse)
    val e = m.asInstanceOf[UnexpectedStatusCodeException]
    e.response shouldBe serviceResponse
    e.req shouldBe "someReq"
  }

  it should "Future.exception a EndpointNotFoundException when pathNotFound" in {
    val m = failer.pathNotFound(serviceRequest)
    val e = m.asInstanceOf[EndpointNotFoundException]
    e.serviceRequest shouldBe serviceRequest
  }

}
