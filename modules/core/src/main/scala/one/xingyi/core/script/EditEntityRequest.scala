package one.xingyi.core.script

import one.xingyi.core.builder.HasId
import one.xingyi.core.http.{Body, FromServiceRequest, ServiceRequest}
import one.xingyi.core.json.{JsonParser, Projection}
import one.xingyi.core.monad.{Monad, MonadCanFailWithException}
import one.xingyi.core.strings.Strings

import scala.language.higherKinds


case class EditEntityRequest[P](entity: P, acceptHeader: Option[String], host: String)
// aha need to be able to make from projection
object EditEntityRequest {
  implicit def hasId[P](implicit hasId: HasId[P, String]): HasId[EditEntityRequest[P], String] = req => hasId(req.entity)

  implicit def hasHost[P]: HasHost[EditEntityRequest[P]] = _.host

  implicit def fromServiceRequest[M[_], Fail, J: JsonParser, SharedP, DomainP]
  (implicit monad: MonadCanFailWithException[M, Fail], failer: EditEntityRequestFailer[Fail], hasId: HasId[DomainP, String], projection: Projection[SharedP, DomainP]): FromServiceRequest[M, EditEntityRequest[DomainP]] = { sr =>
    val name = Strings.lastSection("/")(sr.uri.path.path)
    failer.failOrUseHost(sr) { host =>
      failer.failOrUseBody(sr) { body =>
        val newEntity: DomainP = projection.fromJsonString(body)
        if (name != hasId(newEntity)) monad.fail(failer.failIdDoesntMatch(name, sr)) else
          monad.liftM(EditEntityRequest(newEntity, sr.header("accept"), host))
      }
    }
  }

  implicit def toContentType[P]: ToContentType[EditEntityRequest[P]] = req => req.acceptHeader.getOrElse(DomainDefn.xingyiHeaderPrefix)

}