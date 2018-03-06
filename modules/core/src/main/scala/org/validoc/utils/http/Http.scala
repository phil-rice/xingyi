package org.validoc.utils.http

trait HttpFactory[M[_], HttpReq, HttpRes] extends (ServiceName => HttpReq => M[HttpRes])

trait HttpKlesili[M[_]] {
  protected def httpFactory: HttpFactory[M, ServiceRequest, ServiceResponse]
  def http(name: ServiceName): ServiceRequest => M[ServiceResponse] = httpFactory(name)

}
