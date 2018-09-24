/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.http

import java.io.ByteArrayInputStream
import java.net.URI

import com.sun.net.httpserver.HttpExchange
import one.xingyi.core.UtilsSpec
import one.xingyi.core.language.Language._
import one.xingyi.core.monad.AsyncForScalaFuture.ImplicitsForTest._
import one.xingyi.core.monad.AsyncForScalaFuture._
import org.mockito.Mockito._

import scala.concurrent.Future
class ServiceRequestSpec extends UtilsSpec {

  val serviceRequest = ServiceRequest(Get, Uri("/someUri"), body = Some(Body("someBody")))

  "default to service request  for service request " should "return self" in {
    implicitly[ToServiceRequest[ServiceRequest]].apply(serviceRequest) shouldBe serviceRequest
  }

  "default from serviceRequest  for serviceRequest " should "return self" in {
    implicitly[FromServiceRequest[Future, ServiceRequest]].apply(serviceRequest).await shouldBe serviceRequest
  }

  behavior of "ServiceRequest"

  def sr(list: Header*) = {
    val uri = Uri("/")
    ServiceRequest(Get, uri.domain, uri.path, uri.params, list.toList, None)
  }

  it should "have a constructor that allows the content type and accept header to be easily specified" in {
    ServiceRequest(Get, Uri("/"), Some(AcceptHeader("someAccept")), Some(ContentType("someOtherContent")), List(Header("h1", "v1"), Header("h2", "v2")), None) shouldBe
    sr(ContentType("someOtherContent"), AcceptHeader("someAccept"), Header("h1", "v1"), Header("h2", "v2"))

  }

  it should "allow the accept header to be found: the first in the list of headers with the correct type (it ignores the name)" in {
    sr(Header("h1", "v1"), AcceptHeader("someAccept"), Header("h2", "v2")).accept shouldBe Some(AcceptHeader("someAccept"))
    sr(Header("h1", "v1"), AcceptHeader("someAccept"), AcceptHeader("someOtherAccept"), Header("h2", "v2")).accept shouldBe Some(AcceptHeader("someAccept"))
    sr(Header(Headers.accept, "v1"), AcceptHeader("someAccept"), Header(Headers.accept, "v2")).accept shouldBe Some(AcceptHeader("someAccept"))
    sr(Header(Headers.accept, "v1")).accept shouldBe None
  }

  it should "allow the content type to be found: the first in the list of headers with the correct type (it ignores the name)" in {
    sr(Header("h1", "v1"), ContentType("someContent"), Header("h2", "v2")).contentType shouldBe Some(ContentType("someContent"))
    sr(Header(Headers.contentType, "v1"), ContentType("someContent"), Header(Headers.contentType, "v2")).contentType shouldBe Some(ContentType("someContent"))
    sr(Header(Headers.contentType, "v1"), ContentType("someContent"), ContentType("someOtherContent"), Header(Headers.contentType, "v2")).contentType shouldBe Some(ContentType("someContent"))
    sr(Header(Headers.contentType, "v1")).contentType shouldBe None
  }

  it should "be possible to get a service request from a HttpExchange" in {
    def sr(method: String, uri: String, body: String, headers: (String, String)*): ServiceRequest = {
      val result = mock[HttpExchange]
      when(result.getRequestMethod) thenReturn method
      when(result.getRequestURI) thenReturn new URI(uri)
      when(result.getRequestBody) thenReturn new ByteArrayInputStream(body.getBytes("UTF-8"))
      val javaHeaders = new com.sun.net.httpserver.Headers
      headers.foreach { case (k, v) => javaHeaders.add(k, v) }
      when(result.getRequestHeaders) thenReturn javaHeaders
      implicitly[ToServiceRequest[HttpExchange]] apply result
    }

    sr("get", "/", "") shouldBe ServiceRequest(Get, Uri("/"), List(), None)
    sr("get", "/hello?a=1", "") shouldBe ServiceRequest(Get, Uri("/hello?a=1"), List(), None)
    sr("get", "/hello", "someBody") shouldBe ServiceRequest(Get, Uri("/hello"), List(), Some(Body("someBody")))
    sr("get", "/hello", "someBody", "h1" -> "v1") shouldBe ServiceRequest(Get, Uri("/hello"), List(Header("h1", "v1")), Some(Body("someBody")))
    sr("get", "/hello", "someBody", "h1" -> "v1", "h2" -> "v2") shouldBe ServiceRequest(Get, Uri("/hello"), List(Header("h1", "v1"), Header("h2", "v2")), Some(Body("someBody")))
    sr("get", "/hello", "someBody", "h1" -> "v1", "h1" -> "v2") shouldBe ServiceRequest(Get, Uri("/hello"), List(Header("h1", "v1"),Header("h1", "v2")), Some(Body("someBody")))



    //    object ToServiceRequestForHttpExchange extends ToServiceRequest[HttpExchange] {
    //      override def apply(httpExchange: HttpExchange): ServiceRequest = {
    //        val method = Method(httpExchange.getRequestMethod.toLowerCase())
    //        val body = Body(Streams.readAll(httpExchange.getRequestBody))
    //        val uri = Uri(httpExchange.getRequestURI.toString)
    //        val javaHeaders: Headers = httpExchange.getRequestHeaders;
    //        val headers: immutable.Seq[Header] = javaHeaders.keySet().asScala.foldLeft(List[Header]())((acc, k) => javaHeaders.get(k).asScala.foldLeft(acc)((acc2, v) => Header(k, v) :: acc2))
    //        ServiceRequest(method, uri, headers, Some(body))
    //      }
    //    }

  }

}
