package org.validoc.utils.endpoint

import org.validoc.utils.functions.{Async, Monad, MonadCanFail}
import org.validoc.utils.http._
import org.validoc.utils.language.Language._
import org.validoc.utils.strings.Strings

import scala.annotation.tailrec
import scala.language.higherKinds
import scala.reflect.ClassTag

trait EndpointKleisli[M[_]] {
  protected implicit def monad: Monad[M]
  def endpoint[Req: ClassTag, Res: ClassTag](normalisedPath: String, matchesServiceRequest: MatchesServiceRequest)(raw: Req => M[Res])
                                            (implicit fromServiceRequest: FromServiceRequest[M, Req], toServiceResponse: ToServiceResponse[Res]): ServiceRequest => M[ServiceResponse] =
    EndPoint(normalisedPath, matchesServiceRequest)(raw)
}

trait ChainKleisli[M[_], Fail] {
  protected implicit def monad: MonadCanFail[M, Fail]
  protected def failer: Failer[Fail]

  def chain(endpoints: (ServiceRequest => M[ServiceResponse])*): ServiceRequest => M[ServiceResponse] = { req: ServiceRequest =>
    endpoints.collectFirst {
      case endPoint: PartialFunction[ServiceRequest, M[ServiceResponse]] if endPoint.isDefinedAt(req) => endPoint(req)
      case endPoint if !endPoint.isInstanceOf[PartialFunction[ServiceRequest, M[ServiceResponse]]] => endPoint(req)
    } match {
      case Some(result) => result
      case None => failer.pathNotFound(req).fail
    }
  }

}

case class EndPoint[M[_] : Monad, Req, Res](normalisedPath: String, matchesServiceRequest: MatchesServiceRequest)(kleisli: Req => M[Res])
                                           (implicit fromServiceRequest: FromServiceRequest[M, Req],
                                            toServiceResponse: ToServiceResponse[Res],
                                           ) extends PartialFunction[ServiceRequest, M[ServiceResponse]] {
  def debugInfo(req: ServiceRequest) = s"Endpoint($normalisedPath, $matchesServiceRequest) called with $req results in ${isDefinedAt(req)}"

  override def apply(serviceRequest: ServiceRequest): M[ServiceResponse] = {
    if (isDefinedAt(serviceRequest))
      (fromServiceRequest |==> kleisli |=> toServiceResponse) (serviceRequest)
    else
      throw new IllegalStateException(s"Endpoint $this is not defined at $serviceRequest")
  }
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




