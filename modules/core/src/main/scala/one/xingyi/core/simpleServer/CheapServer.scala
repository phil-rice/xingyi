/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
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
