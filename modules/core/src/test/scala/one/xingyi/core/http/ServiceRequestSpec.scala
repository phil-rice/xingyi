/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.http

import one.xingyi.core.UtilsSpec

import scala.concurrent.Future
import one.xingyi.core.monad.AsyncForScalaFuture._
import ImplicitsForTest._
import one.xingyi.core.language.Language._
class ServiceRequestSpec extends UtilsSpec {

  val serviceRequest = ServiceRequest(Get, Uri("/someUri"), body = Some(Body("someBody")))

  "default to service request  for service request " should "return self" in {
    implicitly[ToServiceRequest[ServiceRequest]].apply(serviceRequest) shouldBe serviceRequest
  }

  "default from serviceRequest  for serviceRequest " should "return self" in {
    implicitly[FromServiceRequest[Future, ServiceRequest]].apply(serviceRequest).await shouldBe serviceRequest
  }

  behavior of "ServiceRequest"

  it should "have a constructor that allows the content type and accept header to be easily specified" in {
    ServiceRequest(Get, Uri("/"), Some(AcceptHeader("someAccept")), Some(ContentType("someOtherContent")), List(Header("h1", "v1"), Header("h2", "v2")), None) shouldBe
    ServiceRequest(Get, Uri("/"), List(ContentType("someOtherContent"), AcceptHeader("someAccept"),Header("h1", "v1"), Header("h2", "v2")),None)

  }

  it should "allow the accept header to be found: the first in the list of headers with the correct type (it ignores the name)" in {
    ServiceRequest(Get, Uri("/"), List(Header("h1", "v1"), AcceptHeader("someAccept"), Header("h2", "v2")), None).accept shouldBe Some(AcceptHeader("someAccept"))
    ServiceRequest(Get, Uri("/"), List(Header("h1", "v1"), AcceptHeader("someAccept"), AcceptHeader("someOtherAccept"), Header("h2", "v2")), None).accept shouldBe Some(AcceptHeader("someAccept"))
    ServiceRequest(Get, Uri("/"), List(Header(Headers.accept, "v1"), AcceptHeader("someAccept"), Header(Headers.accept, "v2")), None).accept shouldBe Some(AcceptHeader("someAccept"))
    ServiceRequest(Get, Uri("/"), List(Header(Headers.accept, "v1")), None).accept shouldBe None
  }

  it should "allow the content type to be found: the first in the list of headers with the correct type (it ignores the name)" in {
    ServiceRequest(Get, Uri("/"), List(Header("h1", "v1"), ContentType("someContent"), Header("h2", "v2")), None).contentType shouldBe Some(ContentType("someContent"))
    ServiceRequest(Get, Uri("/"), List(Header(Headers.contentType, "v1"), ContentType("someContent"), Header(Headers.contentType, "v2")), None).contentType shouldBe Some(ContentType("someContent"))
    ServiceRequest(Get, Uri("/"), List(Header(Headers.contentType, "v1"), ContentType("someContent"), ContentType("someOtherContent"), Header(Headers.contentType, "v2")), None).contentType shouldBe Some(ContentType("someContent"))
    ServiceRequest(Get, Uri("/"), List(Header(Headers.contentType, "v1")), None).contentType shouldBe None
  }


}
