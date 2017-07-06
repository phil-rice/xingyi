package org.validoc.finatra

import com.twitter.finagle.http
import com.twitter.finagle.http.Request
import com.twitter.finatra.http.routing.HttpRouter
import com.twitter.finatra.http.{Controller, HttpServer}
import com.twitter.util.{Duration => TDuration, Future => TFuture, Try => TTry}
import org.validoc.language.ServiceData
import org.validoc.utils.http._
import org.validoc.utils.service.ServicesSummary

class EndpointController(summary: ServicesSummary[TFuture]) extends Controller {
  println(s"EndpointController: $summary")
  println(s"EndpointController endpoints: ${summary.endPoints}")
  summary.endPoints.foreach { endPoint =>
    println(s"Processing endpoint: $endPoint")
    get(endPoint.path) { request: Request =>
      println(s"On endpoint: ${endPoint.path} ${request.uri} ${request.remoteAddress}")
      try {
        val serviceRequest = ServiceRequest(Get, Uri(Protocol("http"), HostName("unknown"), Port(80), Path(request.path)), request.headerMap.get("Accept").map(AcceptHeader(_)))
        println(s"On endpoint: ${endPoint.path} serviceRequest is $serviceRequest")
        endPoint(serviceRequest).map { r: ServiceResponse =>
          response.status(http.Status(r.status.code)).body(r.body.s).contentType(r.contentType.s)
        }
      } catch {
        case e: Throwable => e.printStackTrace(); throw e
      }
    }
  }
}

class StatusController(serviceData: ServiceData[TFuture, _, _]) extends Controller {
}

class PingController extends Controller {
  get("/ping") { request: Request => response.ok("pong").contentType("text/plain") }
}

class FinatraServer(port: Int, controllers: Controller*) extends HttpServer {
  override val modules = Seq()

  override def defaultHttpPort: Int = 9200

  override val defaultFinatraHttpPort: String = s":$port"

  override def configureHttp(router: HttpRouter): Unit = {
    val raw = router
    //      .filter[LoggingMDCFilter[Request, Response]]
    //      .filter[TraceIdMDCFilter[Request, Response]]
    //      .filter[CommonFilters]
    controllers.foldLeft(raw)((acc, c) => acc.add(c))
  }
}


