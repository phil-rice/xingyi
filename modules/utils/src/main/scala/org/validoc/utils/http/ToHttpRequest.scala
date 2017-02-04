package org.validoc.utils.http


trait ToHttpRequest[Req, HttpReq] {
  def toHttpRequest(req: Req): HttpReq

  def toSummary(req: Req): String
}
