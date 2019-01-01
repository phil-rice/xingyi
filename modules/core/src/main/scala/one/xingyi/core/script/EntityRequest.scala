package one.xingyi.core.script

import one.xingyi.core.http._
import one.xingyi.core.json.{JsonWriter, ObjectProjection}
import one.xingyi.core.language.AnyLanguage._
import one.xingyi.core.monad.Monad
import one.xingyi.core.strings.Strings

import scala.language.higherKinds

case class EntityRequest(id: String, hostAndPost: String)
object EntityRequest {
  implicit def fromServiceRequest[M[_] : Monad]: FromServiceRequest[M, EntityRequest] =
    sr => EntityRequest(Strings.lastSection("/")(sr.path.path), sr.host).liftM[M]
}


case class EntityResponse[T](id: String, entity: T)
object EntityResponse {
  implicit def toServiceResponse[J, SharedE, DomainE](implicit jsonWriter: JsonWriter[J], projection: ObjectProjection[SharedE, DomainE]): ToServiceResponse[EntityRequest, ServerPayload[SharedE, DomainE]] =
    cdreq => cdres =>
      ServiceResponse(Status(200), Body(jsonWriter(projection.toJson(cdres.domainObject))), ContentType("text/html"))
}
