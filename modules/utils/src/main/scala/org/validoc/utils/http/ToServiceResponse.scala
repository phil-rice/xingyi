package org.validoc.utils.http

case class ServiceResponse(status: Status, body: Body, contentType: ContentType)

trait ToServiceResponse[HttpRes] {
  def response(httpRes: HttpRes): ServiceResponse
}
