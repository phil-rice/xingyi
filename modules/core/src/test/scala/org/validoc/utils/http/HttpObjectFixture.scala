package org.validoc.utils.http

import org.validoc.utils.functions.Liftable

import org.validoc.utils.language.Language._
import scala.language.higherKinds

trait HttpObjectFixture {

  case class HttpRes(s: String)

  case class HttpReq(s: String)

  type Req = String
  type Res = String

  //    override def toSummary(req: Req): String = s"summary_$req"

  implicit object ToHttpRequestForReq extends ToServiceRequest[Req] {
    override def apply(req: Req): ServiceRequest = ServiceRequest(Get, Uri(req))
  }

  implicit def fromServiceRequestForHttpReq[M[_] : Liftable]: FromServiceRequest[M, HttpReq] = new FromServiceRequestForHttpReq[M]

  class FromServiceRequestForHttpReq[M[_] : Liftable] extends FromServiceRequest[M, HttpReq] {
    override def apply(s: ServiceRequest): M[HttpReq] = HttpReq(s.uri.asUriString).liftM
  }

}