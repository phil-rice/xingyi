package org.validoc.utils.endpoint

import org.validoc.utils.functions.{Liftable, Monad}
import org.validoc.utils.http._
import org.validoc.utils.json.ToJson

import scala.language.higherKinds
import scala.reflect.ClassTag

case class DebugBasePath(path: String) extends AnyVal {
  def withService(serviceId: Int): String = s"$path/$serviceId"

  def withServiceAndEntityAndId(serviceId: Int, entity: String, id: String): String = s"$path/$serviceId/$entity/$id"
}

trait SamplePathOps[T] {
  def samplePath(serviceId: Int)(implicit debugBasePath: DebugBasePath): String

}

trait DebugEndPointReqOps[M[_], T] extends FromServiceRequest[M, T] with SamplePathOps[T]

object DebugEndPointReqOps {
  implicit def endPointReqOpsFromSamplePathOpsAndDebugEndPointsOps[M[_]:Liftable, T](implicit samplePathOps: SamplePathOps[T], fromServiceRequest: FromServiceRequest[M, T]) = new DebugEndPointReqOps[M, T] {
    override def samplePath(serviceId: Int)(implicit debugBasePath: DebugBasePath): String = samplePathOps.samplePath(serviceId)

    override def apply(v1: ServiceRequest) = fromServiceRequest(v1)
  }
}

trait DebugEndPointResOps[T] extends ToServiceResponse[T]

object DebugEndPointResOps {
  implicit def defaultDebugEndPointResOps[T](implicit toJson: ToJson[T]) = new DebugEndPointResOps[T] {
    override def apply(p: T): ServiceResponse = ServiceResponse.fromJson(p)
  }

}


trait DebugEndPointInfo {
  def descriptionOfDebugEndpoint

  def samplePath: String
}

class DebugEndPointService[M[_] : Monad, Req: ClassTag, Res: ClassTag](delegate: (Req => M[Res]) )(implicit debugBasePath: DebugBasePath, debugReq: DebugEndPointReqOps[M, Req], debugRes: DebugEndPointResOps[Res]) extends (Req => M[Res]) with DebugEndPointInfo {
  val fromServiceRequest = implicitly[FromServiceRequest[M, Req]]
  val toServiceResponse = implicitly[ToServiceResponse[Res]]

  //  val debugEndpoint = implicitly[FromServiceRequest[Req]] andThen delegate |=>  toServiceResponse

  override def apply(v1: Req): M[Res] = delegate(v1)

  override def descriptionOfDebugEndpoint: Unit = ???

  override def samplePath: String = ???
}

