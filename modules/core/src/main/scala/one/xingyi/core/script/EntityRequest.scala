package one.xingyi.core.script

import one.xingyi.core.http._
import one.xingyi.core.language.AnyLanguage._
import one.xingyi.core.monad.MonadCanFailWithException
import one.xingyi.core.strings.Strings

import scala.language.higherKinds

case class EntityRequest(id: String, hostAndPost: String)
object EntityRequest {
  implicit def fromServiceRequest[M[_], Fail](implicit monad: MonadCanFailWithException[M, Fail], failer: NoHostFailer[Fail]): FromServiceRequest[M, EntityRequest] =
    sr => failer.failOrUseHost(sr)(host => EntityRequest(Strings.lastSection("/")(sr.path.path), host).liftM[M])
}
