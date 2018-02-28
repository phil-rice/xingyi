package org.validoc.simpleServer

import com.sun.net.httpserver.HttpHandler

class PathAndHandler() extends HttpHandler {
  def path: String
}
