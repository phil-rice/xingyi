package one.xingyi.core.simpleServer

import com.sun.net.httpserver.{Headers, HttpExchange, HttpHandler}
import one.xingyi.core.endpoint.MatchesServiceRequest
import one.xingyi.core.functions.{Async, MonadCanFailWithException}
import one.xingyi.core.http._
import one.xingyi.core._

import scala.language.higherKinds
import one.xingyi.core.language.Language._

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
