package org.validoc.utils.service

import org.validoc.utils.Service
import org.validoc.utils.concurrency.Async
import org.validoc.utils.http._
import org.validoc.utils.json.ToJson
import org.validoc.utils.monads.CanMap._

import scala.language.higherKinds
import scala.reflect.ClassTag

case class DebugBasePath(path: String) extends AnyVal {
  def withService(serviceId: Int): String = s"$path/$serviceId"

  def withServiceAndEntityAndId(serviceId: Int, entity: String, id: String): String = s"$path/$serviceId/$entity/$id"
}

trait SamplePathOps[T] {
  def samplePath(serviceId: Int)(implicit debugBasePath: DebugBasePath): String

}

trait DebugEndPointReqOps[T] extends FromServiceRequest[T] with SamplePathOps[T]

object DebugEndPointReqOps {
  implicit def endPointReqOpsFromSamplePathOpsAndDebugEndPointsOps[T](implicit samplePathOps: SamplePathOps[T], fromServiceRequest: FromServiceRequest[T]) = new DebugEndPointReqOps[T] {
    override def samplePath(serviceId: Int)(implicit debugBasePath: DebugBasePath): String = samplePathOps.samplePath(serviceId)

    override def apply(v1: ServiceRequest): T = fromServiceRequest(v1)
  }
}

trait DebugEndPointResOps[T] extends ToServiceResponse[T]

object DebugEndPointResOps {
  implicit def defaultDebugEndPointResOps[T](implicit toJson: ToJson[T]) = new DebugEndPointResOps[T] {
    override def apply(p: T): ServiceResponse = ServiceResponse.fromJson(p)
  }

}

trait DebugEndPointServiceLanguage[M[_]] extends ServiceComposition[M] {
  /** If you get compilation using this a useful debugging tool is to specificy the Req and Res. Then the compiler will tell you what is missing */
  def debug[Req: FromServiceRequest : DebugEndPointReqOps:ClassTag, Res: DebugEndPointResOps:ClassTag](implicit async: Async[M], debugBasePath: DebugBasePath) =
    service[Req, Res, Req, Res, DebugEndPointService[M, Req, Res]] { (delegate: (Req) => M[Res]) => new DebugEndPointService(delegate) }
}


class DebugEndPointService[M[_] : Async, Req, Res](delegate: Service[M, Req, Res])(implicit debugBasePath: DebugBasePath, debugReq: DebugEndPointReqOps[Req], debugRes: DebugEndPointResOps[Res]) extends Service[M, Req, Res] {
  val fromServiceRequest = implicitly[FromServiceRequest[Req]]
  val toServiceResponse = implicitly[ToServiceResponse[Res]]

  val debugEndpoint = implicitly[FromServiceRequest[Req]] andThen delegate >>> toServiceResponse

  override def apply(v1: Req): M[Res] = delegate(v1)
}

