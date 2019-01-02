package one.xingyi.core.script

import one.xingyi.core.builder.HasId
import one.xingyi.core.http.{Body, FromServiceRequest, ServiceRequest}
import one.xingyi.core.json.{JsonParser, Projection}
import one.xingyi.core.monad.{Monad, MonadCanFailWithException}
import one.xingyi.core.strings.Strings

import scala.language.higherKinds

case class EditEntityException(msg: String) extends RuntimeException(msg)
trait EditEntityRequestFailer[Fail] {
  def failNoJson(sr: ServiceRequest): Fail
  def failNoHost(sr: ServiceRequest): Fail
  def failIdDoesntMatch(id: String, sr: ServiceRequest): Fail
}
object EditEntityRequestFailer {
  implicit object EditEntityRequestFailerForThrowable extends EditEntityRequestFailer[Throwable] {
    override def failNoJson(sr: ServiceRequest): Throwable = EditEntityException(s"No json in the request\n$sr")
    override def failNoHost(sr: ServiceRequest): Throwable = EditEntityException(s"No host in the request\n$sr")
    override def failIdDoesntMatch(id: String, sr: ServiceRequest): Throwable = EditEntityException(s"Id mismatch in url $id\n$sr")
  }
}

case class EditEntityRequest[P](entity: P, acceptHeader: Option[String], host: String)
// aha need to be able to make from projection
object EditEntityRequest {
  implicit def hasId[P](implicit hasId: HasId[P, String]): HasId[EditEntityRequest[P], String] = req => hasId(req.entity)

  implicit def hasHost[P]: HasHost[EditEntityRequest[P]] = _.host

  implicit def fromServiceRequest[M[_], Fail, J: JsonParser, SharedP, DomainP]
  (implicit monad: MonadCanFailWithException[M, Fail], failer: EditEntityRequestFailer[Fail], hasId: HasId[DomainP, String], projection: Projection[SharedP, DomainP]): FromServiceRequest[M, EditEntityRequest[DomainP]] = { sr =>
    val name = Strings.lastSection("/")(sr.uri.path.path)
    (sr.header("host"), sr.body) match {
      case (None, _) => monad.fail(failer.failNoHost(sr))
      case (_, None) => monad.fail(failer.failNoJson(sr))
      case (Some(host), Some(Body(body))) =>
        val newEntity: DomainP = projection.fromJsonString(body)
        if (name != hasId(newEntity)) monad.fail(failer.failIdDoesntMatch(name, sr)) else
          monad.liftM(EditEntityRequest(newEntity, sr.header("accept"), host))
    }
  }

  implicit def toContentType[P]: ToContentType[EditEntityRequest[P]] = req => req.acceptHeader.getOrElse(DomainDefn.xingyiHeaderPrefix)

}