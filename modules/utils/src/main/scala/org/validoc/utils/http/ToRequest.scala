package org.validoc.utils.http


trait ToRequest[Req] {
  def toRequest(req: Req): ServiceRequest
}

case class ServiceRequest(method: Method, uri: Uri, acceptHeader: Option[AcceptHeader]=None, body: Option[Body]=None)