package one.xingyi.utils.simpleServer

import java.util.concurrent.{Callable, ExecutorService, Executors}

import com.sun.net.httpserver.HttpExchange
import one.xingyi.utils.http.{Body, ContentType, ServiceResponse, Status}

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
