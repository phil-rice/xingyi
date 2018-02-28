package org.validoc.simpleServer

import java.net.InetSocketAddress
import java.util.concurrent.Executor

import com.sun.net.httpserver.HttpServer

class SimpleHttpServer (port: Int, executor: Executor, handlers: PathAndHandler*){
  private val server = HttpServer.create(new InetSocketAddress(port), 0)

  handlers.foreach(server.createContext())
  def this {
    this()
    this.server =
    for (pathAndHandler <- handlers) {
      server.createContext(pathAndHandler.path, pathAndHandler)
    }
    server.setExecutor(executor)
  }

  def start(): Unit = {
    server.start()
  }

  def stop(): Unit = {
    server.stop(0)
  }

}
