/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.simpleServer

import java.util.concurrent.{Callable, ExecutorService, Executors}

import com.sun.net.httpserver.HttpExchange
import one.xingyi.core.http.{Body, ContentType, ServiceResponse, Status}

import scala.io.Source

object HttpUtils {
  def makeDefaultExecutor: ExecutorService = Executors.newFixedThreadPool(100)


  def write(exchange: HttpExchange, response: ServiceResponse): Unit = {
    exchange.getResponseHeaders.set("content-type", response.contentType.s)
    val bytes = response.body.s.getBytes("UTF-8")
    exchange.sendResponseHeaders(response.status.code, bytes.length)
    Streams.sendAll(exchange.getResponseBody, bytes)
  }

  def process(exchange: HttpExchange)(response: => Option[ServiceResponse]): Unit = {
    try {
      val result = response
      result match {
        case None => write(exchange, new ServiceResponse(Status(404), Body(s"not found. ${exchange.getRequestURI}"), ContentType("text/plain")))
        case Some(x) => write(exchange, x)
      }
    }
    catch {
      case e: Exception => write(exchange, new ServiceResponse(Status(500), Body(e.getClass.getName + "\n" + e.getMessage), ContentType("text/plain")))
    }
  }


  def id(httpExchange: HttpExchange, prefix: String): String = httpExchange.getRequestURI.getPath.substring(prefix.length + 1)
}
