package org.validoc.utils.endpoint

import org.validoc.utils._
import org.validoc.utils.functions.Monad
import org.validoc.utils.http._
import org.validoc.utils.strings.Strings

import scala.concurrent.Future
import scala.language.higherKinds


trait MatchesServiceRequest {
  def apply(endpointName: String)(serviceRequest: ServiceRequest): Boolean
}

case class EndPoint[M[_] : Monad, Req, Res](normalisedPath: String, matchesServiceRequest: MatchesServiceRequest)(kleisli: Req => M[Res])
                                           (implicit fromServiceRequest: FromServiceRequest[Req], toServiceResponse: ToServiceResponse[Res]) extends (ServiceRequest => Option[M[ServiceResponse]]) {
  override def apply(serviceRequest: ServiceRequest): Option[M[ServiceResponse]] =
    matchesServiceRequest(normalisedPath)(serviceRequest).toOption((fromServiceRequest ~> kleisli |=> toServiceResponse) (serviceRequest))
}

object MatchesServiceRequest {
  def fixedPath(method: Method) = FixedPathAndVerb(method)
  def idAtEnd(method: Method) = IdAtEndAndVerb(method)
}

case class FixedPathAndVerb(method: Method) extends MatchesServiceRequest {
  override def apply(endpointName: String)(serviceRequest: ServiceRequest): Boolean = serviceRequest.method == method && serviceRequest.uri.asUriString == endpointName
}

case class IdAtEndAndVerb(method: Method) extends MatchesServiceRequest {
  val startFn = Strings.allButlastSection("/") _
  override def apply(endpointName: String)(serviceRequest: ServiceRequest): Boolean = startFn(serviceRequest.uri.asUriString) == endpointName && serviceRequest.method == method
}

import org.validoc.utils.concurrency.AsyncForScalaFuture._
import org.validoc.utils.concurrency.AsyncForScalaFuture.ImplicitsForTest._

object EndPointChecker {

  implicit object F extends FromServiceRequest[Int] {
    override def apply(v1: ServiceRequest): Int = ???
  }

  implicit object G extends ToServiceResponse[Int] {
    override def apply(v1: Int): ServiceResponse = ???
  }

  def raw(x: Int) = Future.successful(x + 1)
  val x = EndPoint[Future, Int, Int]("/path", IdAtEndAndVerb(Get)) _ <++> EndPoint[Future, Int, Int]("/path", IdAtEndAndVerb(Get)) _

}




