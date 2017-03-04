package org.validoc.utils.http

import org.validoc.utils.service.ServerContext


/** Typically used when calling out to another microservice */
trait ToServiceRequest[Req] extends (Req => ServiceRequest)

object ToServiceRequest {
  implicit def ToServiceRequestFrom[HttpReq](implicit serverContext: ServerContext[HttpReq, _]) = serverContext.toServiceRequest
}

/** Typically used when being called by the web framework */
trait FromServiceRequest[Req] extends (ServiceRequest => Req)

object FromServiceRequest {
  implicit def FromServiceRequest[HttpRes](implicit serverContext: ServerContext[_, HttpRes]) = serverContext.fromServiceRequest
}

case class ServiceRequest(method: Method, uri: Uri, acceptHeader: Option[AcceptHeader] = None, otherHeaders: List[Header] = List(), body: Option[Body] = None)