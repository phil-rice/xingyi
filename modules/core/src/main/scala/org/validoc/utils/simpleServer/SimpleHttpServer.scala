package org.validoc.utils.simpleServer

import java.net.InetSocketAddress
import java.util.concurrent.Executor

import com.sun.net.httpserver.{HttpHandler, HttpServer}

class SimpleHttpServer(port: Int, handler: HttpHandler)(implicit executor: Executor) {
  private val server = HttpServer.create(new InetSocketAddress(port), 0)
  server.createContext("/", handler)
  server.setExecutor(executor)

  def start(): Unit = {
    server.start()
  }

  def stop(): Unit = {
    server.stop(0)
  }

}
