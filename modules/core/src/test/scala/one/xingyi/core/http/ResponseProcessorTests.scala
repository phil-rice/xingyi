/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
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
