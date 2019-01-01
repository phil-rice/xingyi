package one.xingyi.scriptSharedBackend.domain
import one.xingyi.core.builder.HasId
import one.xingyi.core.http.FromServiceRequest
import one.xingyi.core.json.{JsonParser, Projection}
import one.xingyi.core.monad.Monad
import one.xingyi.core.script.{DomainDefn, HasHost, ToContentType}
import one.xingyi.core.strings.Strings

import scala.language.higherKinds

case class EditEntityRequest[P](entity: P, xingYiHeader: Option[String], host: String)
// aha need to be able to make from projection
object EditEntityRequest {
  implicit def hasId[P](implicit hasId: HasId[P, String]): HasId[EditEntityRequest[P], String] = req => hasId(req.entity)

  implicit def hasHost[P]: HasHost[EditEntityRequest[P]] = _.host

  implicit def fromServiceRequest[M[_], J: JsonParser, SharedP, DomainP](implicit monad: Monad[M], hasId: HasId[DomainP, String], projection: Projection[SharedP, DomainP]): FromServiceRequest[M, EditEntityRequest[DomainP]] = { sr =>
    val name = Strings.lastSection("/")(sr.uri.path.path)
    val newEntity: DomainP = projection.fromJsonString(sr.body.getOrElse(throw new RuntimeException("cannot create as body of request empty")).s)
    if (name != hasId(newEntity)) throw new RuntimeException(s"Cannot edit name. Name in request is $name. Request is $sr Object is $newEntity")
    monad.liftM(EditEntityRequest(newEntity, sr.header("xingyi"), sr.host))
  }

  implicit def toContentType[P]: ToContentType[EditEntityRequest[P]] = req => req.xingYiHeader.getOrElse(DomainDefn.xingyiHeaderPrefix)

}