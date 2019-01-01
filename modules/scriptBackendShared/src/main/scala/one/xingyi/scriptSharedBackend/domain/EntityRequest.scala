package one.xingyi.scriptSharedBackend.domain
import one.xingyi.core.http._
import one.xingyi.core.json.{JsonWriter, ObjectProjection}
import one.xingyi.core.monad.Monad
import one.xingyi.core.script.ServerPayload
import one.xingyi.core.strings.Strings
import one.xingyi.core.language.AnyLanguage._

import scala.language.higherKinds

case class EntityRequest(id: String, hostAndPost: String)
object EntityRequest {
  implicit def fromServiceRequest[M[_] : Monad]: FromServiceRequest[M, EntityRequest] =
    sr => EntityRequest(Strings.lastSection("/")(sr.path.path), sr.host).liftM[M]
}


case class EntityResponse[T](id: String, entity: T)
object EntityResponse {
  implicit def toServiceResponse[J, Shared, Domain](implicit jsonWriter: JsonWriter[J], projection: ObjectProjection[Shared, Domain]): ToServiceResponse[EntityRequest, ServerPayload[Domain]] =
    cdreq => cdres =>
      ServiceResponse(Status(200), Body(jsonWriter(projection.toJson(cdres.domainObject))), ContentType("text/html"))
}
