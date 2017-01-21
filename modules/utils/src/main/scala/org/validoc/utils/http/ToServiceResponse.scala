package org.validoc.utils.http

case class ServiceResponse(status: Status, body: Body, contentType: ContentType)

trait ToServiceResponse[HttpRes] {
  def status(res: HttpRes): Status

  def body(res: HttpRes): Body

  def contentType(res: HttpRes): ContentType

  def response (httpRes: HttpRes)= ServiceResponse(status(httpRes), body(httpRes), contentType(httpRes))
}
