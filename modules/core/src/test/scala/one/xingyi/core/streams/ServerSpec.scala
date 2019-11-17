/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.streams
import java.util.concurrent.Executors

import com.sun.net.httpserver.{HttpExchange, HttpHandler}
import javax.net.ssl.SSLContext
import one.xingyi.core.UtilsSpec
import one.xingyi.core.client.HttpClient
import one.xingyi.core.http._
import one.xingyi.core.monad.IdentityMonad
import one.xingyi.core.simpleServer.{EndpointHandler, HttpUtils, SimpleHttpServer}
import org.mockito.{ArgumentCaptor, Matchers}
import org.mockito.Mockito._
import one.xingyi.core.language.Language._

import scala.util.Success

class ServerSpec extends UtilsSpec {

  behavior of "Server"

  implicit val executors = HttpUtils.makeDefaultExecutor


  it should "start and stop lots of times" in {
    val handler = mock[HttpHandler]
    (1 to 3) foreach { i =>
      val s = new SimpleHttpServer(10000, handler)
      s.start()
      s.stop()
    }
  }


  it should "send post requests to the handler" in {
    implicit val sslContent: Option[SSLContext] = None
    val fn = mock[ServiceRequest => IdentityMonad[Option[ServiceResponse]]]
    var handler = new EndpointHandler[IdentityMonad, Throwable](fn)
    val port = 10000
    val s = new SimpleHttpServer(port, handler)
    s.start()
    try {
      val client = HttpClient[IdentityMonad](Domain(Protocol("http"), HostName("localhost"), Port(port)))
      val serviceRequest = ServiceRequest(Post, Uri(s"http://localhost:$port/hello"), List(Header("h1", "v1")), Some(Body("someBody")))
      val serviceResponse = ServiceResponse("someHtml")
      val captor = ArgumentCaptor.forClass(classOf[ServiceRequest])
      when(fn.apply(captor.capture)).thenReturn(Option(serviceResponse).liftM[IdentityMonad])
      val Success(response) = client(serviceRequest).value

      ServiceRequest.removeHeader("user-agent")(captor.getValue) shouldBe ServiceRequest(Post, None, Path("/hello"), List(),
        List(SimpleHeader("accept", "text/html, image/gif, image/jpeg, *; q=.2, */*; q=.2"),
          SimpleHeader("connection", "keep-alive"), SimpleHeader("h1", "v1"), SimpleHeader("host", "localhost:10000"),
          SimpleHeader("content-type", "application/x-www-form-urlencoded"), SimpleHeader("content-length", "8")), Some(Body("someBody")))

      ServiceResponse.removeHeader("Date")(response) shouldBe ServiceResponse(Status(200), Body("someHtml"), List(
        SimpleHeader(null, "HTTP/1.1 200 OK"),
        SimpleHeader("Content-type", "text/html"),
        SimpleHeader("Content-length", "8")))


    } finally {
      s.stop()
    }
  }

  it should "send requests to the handler without a body" in {
    implicit val sslContent: Option[SSLContext] = None
    val fn = mock[ServiceRequest => IdentityMonad[Option[ServiceResponse]]]
    var handler = new EndpointHandler[IdentityMonad, Throwable](fn)
    val port = 10000
    val s = new SimpleHttpServer(port, handler)
    s.start()
    try {
      val client = HttpClient[IdentityMonad](Domain(Protocol("http"), HostName("localhost"), Port(port)))
      val serviceRequest = ServiceRequest(Get, Uri(s"http://localhost:$port/hello"), List(Header("h1", "v1")), None)
      val serviceResponse = ServiceResponse("someHtml")
      val captor = ArgumentCaptor.forClass(classOf[ServiceRequest])
      when(fn.apply(captor.capture)).thenReturn(Option(serviceResponse).liftM[IdentityMonad])
      val Success(response) = client(serviceRequest).value

      ServiceRequest.removeHeader("user-agent")(captor.getValue) shouldBe ServiceRequest(Get, None, Path("/hello"), List(),
        List(SimpleHeader("accept", "text/html, image/gif, image/jpeg, *; q=.2, */*; q=.2"),
          SimpleHeader("connection", "keep-alive"), SimpleHeader("h1", "v1"), SimpleHeader("host", "localhost:10000")), None)

      ServiceResponse.removeHeader("Date")(response) shouldBe ServiceResponse(Status(200), Body("someHtml"), List(
        SimpleHeader(null, "HTTP/1.1 200 OK"),
        SimpleHeader("Content-type", "text/html"),
        SimpleHeader("Content-length", "8")))


    } finally {
      s.stop()
    }
  }
  it should "deal with 404s" in {
    implicit val sslContent: Option[SSLContext] = None
    val fn = mock[ServiceRequest => IdentityMonad[Option[ServiceResponse]]]
    var handler = new EndpointHandler[IdentityMonad, Throwable](fn)
    val port = 10000
    val s = new SimpleHttpServer(port, handler)
    s.start()
    try {
      val client = HttpClient[IdentityMonad](Domain(Protocol("http"), HostName("localhost"), Port(port)))
      val serviceRequest = ServiceRequest(Get, Uri(s"http://localhost:$port/hello"), List(Header("h1", "v1")), None)
      val captor = ArgumentCaptor.forClass(classOf[ServiceRequest])
      when(fn.apply(captor.capture)).thenReturn(Option[ServiceResponse](null).liftM[IdentityMonad])
      val Success(response) = client(serviceRequest).value

      ServiceRequest.removeHeader("user-agent")(captor.getValue) shouldBe ServiceRequest(Get, None, Path("/hello"), List(),
        List(SimpleHeader("accept", "text/html, image/gif, image/jpeg, *; q=.2, */*; q=.2"),
          SimpleHeader("connection", "keep-alive"), SimpleHeader("h1", "v1"), SimpleHeader("host", "localhost:10000")), None)

      val body = response.body.asUtf
      body should include ("FileNotFoundException")
      ServiceResponse.removeHeader("Date")(response.copy(body=Body(""))) shouldBe ServiceResponse(Status(404), Body(""), List())


    } finally {
      s.stop()
    }
  }
  it should "deal with 500s" in {
    implicit val sslContent: Option[SSLContext] = None
    val fn = mock[ServiceRequest => IdentityMonad[Option[ServiceResponse]]]
    var handler = new EndpointHandler[IdentityMonad, Throwable](fn)
    val port = 10000
    val s = new SimpleHttpServer(port, handler)
    s.start()
    val runtimeException = new RuntimeException("SomeMessage")
    try {
      val client = HttpClient[IdentityMonad](Domain(Protocol("http"), HostName("localhost"), Port(port)))
      val serviceRequest = ServiceRequest(Get, Uri(s"http://localhost:$port/hello"), List(Header("h1", "v1")), None)
      val captor = ArgumentCaptor.forClass(classOf[ServiceRequest])
      when(fn.apply(captor.capture)).thenReturn(runtimeException.liftException[IdentityMonad, Option[ServiceResponse]])
      val Success(response) = client(serviceRequest).value

      ServiceRequest.removeHeader("user-agent")(captor.getValue) shouldBe ServiceRequest(Get, None, Path("/hello"), List(),
        List(SimpleHeader("accept", "text/html, image/gif, image/jpeg, *; q=.2, */*; q=.2"),
          SimpleHeader("connection", "keep-alive"), SimpleHeader("h1", "v1"), SimpleHeader("host", "localhost:10000")), None)


      val body = response.body.asUtf
      body should include ("java.lang.RuntimeException\nSomeMessage")

      ServiceResponse.removeHeader("Date")(response.copy(body = Body(""))) shouldBe ServiceResponse(Status(500), Body(""), List())


    } finally {
      s.stop()
    }
  }

}
