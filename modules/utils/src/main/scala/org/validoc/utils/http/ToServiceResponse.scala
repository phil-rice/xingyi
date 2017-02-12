package org.validoc.utils.http

case class ServiceResponse(status: Status, body: Body, contentType: ContentType)

trait ToServiceResponse[HttpRes] extends (HttpRes => ServiceResponse)

trait FromServiceResponse[HttpRes] extends (ServiceResponse => HttpRes)
