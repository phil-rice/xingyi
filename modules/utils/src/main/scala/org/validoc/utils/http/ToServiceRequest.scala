package org.validoc.utils.http


/** Typically used when calling out to another microservice */
trait ToServiceRequest[Req] extends (Req => ServiceRequest)

/** Typically used when being called by the web framework */
trait FromServiceRequest[Req] extends (ServiceRequest => Req)

case class ServiceRequest(method: Method, uri: Uri, acceptHeader: Option[AcceptHeader]=None, otherHeaders: List[Header] = List(), body: Option[Body]=None)