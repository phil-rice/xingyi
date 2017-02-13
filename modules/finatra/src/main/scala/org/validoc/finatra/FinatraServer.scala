package org.validoc.finatra

import java.nio.file.Paths

import com.twitter.finagle.http.{Request, Response}
import com.twitter.finatra.http.{Controller, HttpServer}
import com.twitter.finatra.http.filters.{CommonFilters, LoggingMDCFilter, TraceIdMDCFilter}
import com.twitter.finatra.http.routing.HttpRouter
import com.twitter.logging.Logging
import org.slf4j.LoggerFactory


class FinatraServer(port: Int, controllers: Controller*) extends HttpServer {


  override val modules = Seq()

  override def defaultHttpPort: Int = 9200

  override val defaultFinatraHttpPort: String = s":$port"

  override def configureHttp(router: HttpRouter): Unit = {
    val raw = router
      .filter[LoggingMDCFilter[Request, Response]]
      .filter[TraceIdMDCFilter[Request, Response]]
      .filter[CommonFilters]
    controllers.foldLeft(raw)((acc, c) => acc.add(c))
  }
}


