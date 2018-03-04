package org.validoc.finatra

import com.twitter.finagle.http.{Request, Response}
import com.twitter.finatra.http.filters.{CommonFilters, LoggingMDCFilter, TraceIdMDCFilter}
import com.twitter.finatra.http.routing.HttpRouter
import com.twitter.finatra.http.{Controller, HttpServer}
import com.twitter.util.{Duration => TDuration, Future => TFuture, Try => TTry}


//
//class StatusController(serviceData: ServiceData[TFuture, _, _]) extends Controller {
//}

class PingController extends Controller {
  get("/ping") { request: Request => response.ok("pong").contentType("text/plain") }
}

class FinatraServer(port: Int, controllers: Controller*) extends HttpServer {
  override val modules = Seq()

  //  override def defaultHttpPort: Int = 9200

  override val defaultFinatraHttpPort: String = s":$port"

  override def configureHttp(router: HttpRouter): Unit = {
    val raw = router
          .filter[LoggingMDCFilter[Request, Response]]
          .filter[TraceIdMDCFilter[Request, Response]]
          .filter[CommonFilters]
    controllers.foreach(raw.add)
  }
}




