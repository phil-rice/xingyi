package one.xingyi.core.streams
import java.util.concurrent.Executors

import com.sun.net.httpserver.{HttpExchange, HttpHandler}
import one.xingyi.core.UtilsSpec
import one.xingyi.core.simpleServer.SimpleHttpServer

class RememberHttpHandler extends HttpHandler {
  override def handle(httpExchange: HttpExchange): Unit = {

  }
}
class ServerSpec extends UtilsSpec {

  behavior of "Server"

  implicit val executors = Executors.newFixedThreadPool(3)

  val handler: HttpHandler = new HttpHandler {
    override def handle(httpExchange: HttpExchange): Unit = {
    }
  }

  it should "start and stop lots of times" in {
    (1 to 3) foreach { i =>
      val s = new SimpleHttpServer(10000, handler)
      s.start()
      s.stop()
    }
  }

}
