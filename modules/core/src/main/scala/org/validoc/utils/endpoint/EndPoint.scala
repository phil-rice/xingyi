package org.validoc.utils.endpoint

import org.validoc.utils.functions.Monad
import org.validoc.utils.http._
import org.validoc.utils.language.Language._
import org.validoc.utils.strings.Strings

import scala.language.higherKinds

case class EndPoint[M[_] : Monad, Req, Res](normalisedPath: String, matchesServiceRequest: MatchesServiceRequest)(kleisli: Req => M[Res])
                                           (implicit fromServiceRequest: FromServiceRequest[M, Req], toServiceResponse: ToServiceResponse[Res]) extends (ServiceRequest => Option[M[ServiceResponse]]) {
  def debugInfo(req: ServiceRequest) = s"Endpoint($normalisedPath, $matchesServiceRequest) called with $req results in ${isDefinedAt(req)}"

  override def apply(serviceRequest: ServiceRequest): Option[M[ServiceResponse]] =
    isDefinedAt(serviceRequest).toOption((fromServiceRequest |==> kleisli |=> toServiceResponse) (serviceRequest))
  def isDefinedAt(serviceRequest: ServiceRequest): Boolean = matchesServiceRequest(normalisedPath)(serviceRequest)
  override def toString() = s"Endpoint($normalisedPath, $matchesServiceRequest)"
}

trait MatchesServiceRequest {
  def method: Method
  def apply(endpointName: String)(serviceRequest: ServiceRequest): Boolean
}


object MatchesServiceRequest {
  def fixedPath(method: Method) = FixedPathAndVerb(method)
  def idAtEnd(method: Method) = IdAtEndAndVerb(method)
}

case class FixedPathAndVerb(method: Method) extends MatchesServiceRequest {
  override def apply(endpointName: String)(serviceRequest: ServiceRequest): Boolean = serviceRequest.method == method && serviceRequest.uri.path.asUriString == endpointName
}

case class IdAtEndAndVerb(method: Method) extends MatchesServiceRequest {
  val startFn = Strings.allButlastSection("/") _
  override def apply(endpointName: String)(serviceRequest: ServiceRequest): Boolean = startFn(serviceRequest.uri.path.asUriString) == endpointName && serviceRequest.method == method
}




