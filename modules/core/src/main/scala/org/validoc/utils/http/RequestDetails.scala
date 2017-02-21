package org.validoc.utils.http

object RequestDetails {
  def apply[Req](name: String)(req: Req): RequestDetails[Req] = RequestDetails(req, s"Calling $name with $req")

}

case class RequestDetails[Req](req: Req, requestSummary: String)
