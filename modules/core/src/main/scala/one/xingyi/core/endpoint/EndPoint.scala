package one.xingyi.core.endpoint

import one.xingyi.core.functions.{Async, Monad, MonadCanFail}
import one.xingyi.core.http._
import one.xingyi.core.language.Language._
import one.xingyi.core.strings.Strings

import scala.annotation.tailrec
import scala.language.higherKinds
import scala.reflect.ClassTag

trait EndpointKleisli[M[_]] {
  protected implicit def monad: Monad[M]
  def endpoint[Req: ClassTag, Res: ClassTag](normalisedPath: String, matchesServiceRequest: MatchesServiceRequest)(raw: Req => M[Res])
                                            (implicit fromServiceRequest: FromServiceRequest[M, Req], toServiceResponse: ToServiceResponse[Res]): ServiceRequest => M[Option[ServiceResponse]] =
    EndPoint(normalisedPath, matchesServiceRequest)(raw)
}




trait ChainKleisli[M[_], Fail] {
  protected implicit def monad: MonadCanFail[M, Fail]
  protected def failer: Failer[Fail]

  def chain(chains: (ServiceRequest => M[Option[ServiceResponse]])*): ServiceRequest => M[Option[ServiceResponse]] = { serviceRequest: ServiceRequest =>
    chains.foldLeft[M[Option[ServiceResponse]]](monad.liftM(Option.empty[ServiceResponse])) {
      case (acc, v) => acc.flatMap[Option[ServiceResponse]] {
        _ match {
          case s if s.isDefined => monad.liftM(s)
          case none => v match {
            case pf: PartialFunction[ServiceRequest, _] =>
              if (pf.isDefinedAt(serviceRequest))
                v(serviceRequest)
              else
                monad.liftM(None)
            case _ =>
              v(serviceRequest)
          }
        }
      }
    }
  }
}
case class EndPoint[M[_] : Monad, Req, Res](normalisedPath: String, matchesServiceRequest: MatchesServiceRequest)(kleisli: Req => M[Res])
                                           (implicit fromServiceRequest: FromServiceRequest[M, Req],
                                            toServiceResponse: ToServiceResponse[Res],
                                           ) extends PartialFunction[ServiceRequest, M[Option[ServiceResponse]]] {
  def debugInfo(req: ServiceRequest) = s"Endpoint($normalisedPath, $matchesServiceRequest) called with $req results in ${isDefinedAt(req)}"

  override def apply(serviceRequest: ServiceRequest): M[Option[ServiceResponse]] = {
    if (isDefinedAt(serviceRequest))
      (fromServiceRequest |==> kleisli |=> toServiceResponse |=> toSome) (serviceRequest)
    else
      Option.empty[ServiceResponse].liftM
  }
  def isDefinedAt(serviceRequest: ServiceRequest): Boolean = {
    matchesServiceRequest(normalisedPath)(serviceRequest)
  }
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
  override def apply(endpointName: String)(serviceRequest: ServiceRequest): Boolean =
    serviceRequest.method == method && serviceRequest.uri.path.asUriString == endpointName
}

case class IdAtEndAndVerb(method: Method) extends MatchesServiceRequest {
  val startFn = Strings.allButlastSection("/") _
  override def apply(endpointName: String)(serviceRequest: ServiceRequest): Boolean = startFn(serviceRequest.uri.path.asUriString) == endpointName && serviceRequest.method == method
}




