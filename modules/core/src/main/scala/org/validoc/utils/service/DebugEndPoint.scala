package org.validoc.utils.service

import org.validoc.utils.Service
import org.validoc.utils.concurrency.Async
import org.validoc.utils.http._
import org.validoc.utils.json.ToJson
import org.validoc.utils.monads.CanMap._

import scala.language.higherKinds

case class DebugBasePath(path: String) extends AnyVal {
  def withService(id: Int): String = s"$path/id"
}

trait DebugEndPointReqOps[T] extends FromServiceRequest[T] {
  def samplePath(serviceId: Int)(implicit debugBasePath: DebugBasePath): String
}

trait DebugEndPointResOps1[T] extends ToServiceResponse[T]

object DebugEndPointResOps1 {
  implicit def defaultDebugEndPointResOps[T](implicit toJson: ToJson[T]) = new DebugEndPointResOps1[T] {
    override def apply(p: T): ServiceResponse = ServiceResponse.fromJson(p)
  }

}

trait DebugEndPointServiceLanguage[M[_]] extends ServiceComposition[M] {
  def debug[Req: FromServiceRequest : DebugEndPointReqOps, Res: DebugEndPointResOps1](implicit async: Async[M], debugBasePath: DebugBasePath) =
    service[Req, Res, Req, Res, DebugEndPointService[M, Req, Res]] { (delegate: (Req) => M[Res]) => new DebugEndPointService(delegate) }
}

class DebugEndPointService[M[_] : Async, Req, Res](delegate: Service[M, Req, Res])(implicit debugBasePath: DebugBasePath, debugReq: DebugEndPointReqOps[Req], debugRes: DebugEndPointResOps1[Res]) extends Service[M, Req, Res] {
  val fromServiceRequest = implicitly[FromServiceRequest[Req]]
  val toServiceResponse = implicitly[ToServiceResponse[Res]]

  val debugEndpoint = (implicitly[FromServiceRequest[Req]] andThen delegate >>> toServiceResponse)

  override def apply(v1: Req): M[Res] = delegate(v1)
}

