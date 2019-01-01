package one.xingyi.scriptSharedBackend.domain
import one.xingyi.core.builder.HasId
import one.xingyi.core.http.FromServiceRequest
import one.xingyi.core.monad.Monad
import one.xingyi.core.script.{DomainDefn, HasHost, ToContentType}
import one.xingyi.core.strings.Strings

import scala.language.higherKinds

case class GetEntityRequest(id: String, host: String, xingYiHeader: Option[String])
object GetEntityRequest {
  implicit def hasId: HasId[GetEntityRequest, String] = _.id
  implicit def hasHost: HasHost[GetEntityRequest] = _.host

  implicit def fromServiceRequest[M[_]](implicit monad: Monad[M]): FromServiceRequest[M, GetEntityRequest] =
    sr => monad.liftM(GetEntityRequest(Strings.lastSection("/")(sr.uri.path.path), sr.host, sr.header("accept")))

  implicit def toContentType: ToContentType[GetEntityRequest] = req => req.xingYiHeader.getOrElse(DomainDefn.xingyiHeaderPrefix)
}