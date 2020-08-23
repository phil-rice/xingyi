/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.http

import one.xingyi.core.UtilsSpec

import scala.concurrent.Future

import org.mockito.Mockito._

class HttpFactorySpec extends UtilsSpec {

  behavior of "HttpKleisli"

  val serviceName = ServiceName("someName")
  val httpService = mock[ServiceRequest => Future[ServiceResponse]]

  it should "get the http kleisli from the factory" in {
    val mockHttpFactory = mock[HttpFactory[Future, ServiceRequest, ServiceResponse]]
    val httpKlesili = new HttpKlesili[Future] {
      override protected def httpFactory: HttpFactory[Future, ServiceRequest, ServiceResponse] = mockHttpFactory
    }
    when(mockHttpFactory.apply(serviceName)) thenReturn (httpService)
    httpKlesili.http(serviceName) shouldBe httpService

  }

}
