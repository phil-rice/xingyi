package org.validoc.utils.http

case class ServiceRequest(method: Method, uri: Uri, acceptHeader: Option[AcceptHeader] = None, otherHeaders: List[Header] = List(), body: Option[Body] = None)