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
                                            (implicit fromServiceRequest: FromServiceRequest[M, Req], toServiceResponse: ToServiceResponse[Res]): EndPoint[M, Req, Res] =
    EndPoint(normalisedPath, matchesServiceRequest)(raw)
}

trait ChainKleisli[M[_], Fail] {
  protected implicit def monad: MonadCanFail[M, Fail]
  protected def failer: Failer[Fail]

  def chain(endpoints: EndPoint[M, _, _]*): ServiceRequest => M[ServiceResponse] = { req: ServiceRequest =>
    @tailrec
    def recurse(seq: List[EndPoint[M, _, _]]): M[ServiceResponse] = {

      seq match {
        case head :: tail =>
          println(head.asInstanceOf[EndPoint[M, _, _]].debugInfo(req))
          head(req) match {
            case Some(result) => result
            case None => recurse(tail)
          }
        case Nil => failer.pathNotFound(req).fail[M, ServiceResponse]
      }
    }
    recurse(endpoints.toList)
  }

}

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




