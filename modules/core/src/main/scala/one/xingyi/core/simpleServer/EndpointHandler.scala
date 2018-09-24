/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.simpleServer

import com.sun.net.httpserver.{Headers, HttpExchange, HttpHandler}
import one.xingyi.core.http._
import one.xingyi.core.language.Language._
import one.xingyi.core.monad.{Async, MonadCanFailWithException}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.language.higherKinds

class EndpointHandler[M[_] : Async, Fail](fn: ServiceRequest => M[Option[ServiceResponse]])(implicit monadCanFail: MonadCanFailWithException[M, Fail], failer: Failer[Fail]) extends HttpHandler {
  override def handle(httpExchange: HttpExchange) = {
    HttpUtils.process(httpExchange) {
      val method = Method(httpExchange.getRequestMethod.toLowerCase())
      val body = Body(Streams.readAll(httpExchange.getRequestBody))
      val uri = Uri(httpExchange.getRequestURI.toString)
      val headers: Headers = httpExchange.getRequestHeaders;
      val contentType = Option(headers.getFirst("content-type")).map(ContentType(_))
      val serviceRequest = ServiceRequest(method, uri, contentType = contentType, body = Some(body))
      fn(serviceRequest).await()
    }
  }
}
