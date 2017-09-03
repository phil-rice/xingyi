package org.validoc.finatra

import com.twitter.finagle.http
import com.twitter.finagle.http.Request
import com.twitter.finatra.http.routing.HttpRouter
import com.twitter.finatra.http.{Controller, HttpServer}
import com.twitter.util.{FuturePool, Duration => TDuration, Future => TFuture, Try => TTry}
import org.validoc.utils.Closable
import org.validoc.utils.http._
import org.validoc.utils.server.ServerBuilder
import org.validoc.utils.serviceTree.{ServiceDescription, ServiceTree}


//
//class StatusController(serviceData: ServiceData[TFuture, _, _]) extends Controller {
//}

class PingController extends Controller {
  get("/ping") { request: Request => response.ok("pong").contentType("text/plain") }
}

object FinatraServer {

  implicit object ClosableForFinatraServer extends Closable[FinatraServer] {
    override def close(t: FinatraServer): Unit = t.close()
  }

  implicit def serverBuilder(implicit futurePool: FuturePool) = new ServerBuilder[TFuture, FinatraServer] {
    override def apply(port: Int, serviceTrees: List[ServiceTree[TFuture, _, _, ServiceDescription]]) = {
      val server = new FinatraServer(port, new PingController, new EndpointController(serviceTrees))
      futurePool(server.nonExitingMain(Array()))
      println(s"Building server: $server $port")
      server
    }
  }

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
    controllers.foreach(raw.add)
  }
}




