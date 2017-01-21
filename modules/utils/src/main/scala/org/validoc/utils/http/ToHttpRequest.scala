package org.validoc.utils.http


trait ToHttpRequest[Req, HttpReq] {
  def toHttpRequest(req: Req): HttpReq

  def toHost(req: Req): String

  def toUri(req: Req): String

  def toAcceptHeader(req: Req): String

  def toSummary(req: Req) = s"Host=${toHost(req)}, url = ${toUri(req)}, acceptHeaders = ${toAcceptHeader(req)}"
}
