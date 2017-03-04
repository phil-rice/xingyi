package org.validoc.utils.http

import org.validoc.utils.service.ServerContext

case class ServiceRequest(method: Method, uri: Uri, acceptHeader: Option[AcceptHeader] = None, otherHeaders: List[Header] = List(), body: Option[Body] = None)