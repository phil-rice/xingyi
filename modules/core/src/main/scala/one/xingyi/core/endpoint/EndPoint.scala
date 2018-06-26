/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.endpoint

import one.xingyi.core.http._
import one.xingyi.core.language.Language._
import one.xingyi.core.monad.{Monad, MonadCanFail}
import one.xingyi.core.strings.Strings

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




