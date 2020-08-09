package one.xingyi.core.simpleServer

import java.util.concurrent.{Executors, TimeUnit}

import one.xingyi.core.endpoint.ChainKleisli
import one.xingyi.core.http._
import one.xingyi.core.monad.{Async, MonadCanFailWithException}

import scala.language.higherKinds


object CheapServer {
  def apply[M[_] : Async, Fail: Failer]
  (port: Port, endPoints: ServiceRequest => M[Option[ServiceResponse]], stopAtEnd: Boolean = false)
  (block: => Unit)
  (implicit monad: MonadCanFailWithException[M, Fail]): Unit = {
    val cheapServer = new CheapServer[M, Fail](port.port, endPoints)
    val server = cheapServer.start
    try {
      block
    } finally {
      if (stopAtEnd) cheapServer.stop(server)
    }
  }
}

class CheapServer[M[_] : Async, Fail: Failer](port: Int, endpoints: (ServiceRequest => M[Option[ServiceResponse]])*)
                                             (implicit monad: MonadCanFailWithException[M, Fail]) extends ChainKleisli[M] {

  implicit val executors = Executors.newFixedThreadPool(10)

  def start: SimpleHttpServer = {
    val server = new SimpleHttpServer(port, new EndpointHandler[M, Fail](chain(endpoints: _*)))
    server.start()
    server
  }
  def stop(server: SimpleHttpServer) = {
    server.stop()
    executors.shutdownNow()
    executors.awaitTermination(1000, TimeUnit.MILLISECONDS)
  }
}