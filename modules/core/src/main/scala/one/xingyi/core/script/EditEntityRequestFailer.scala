package one.xingyi.core.script
import one.xingyi.core.http.{Body, ServiceRequest}
import one.xingyi.core.monad.MonadCanFailWithException
import one.xingyi.core.language.AnyLanguage._

import scala.language.higherKinds
case class EditEntityException(msg: String) extends RuntimeException(msg)

trait NoHostFailer[Fail] {
  def failNoHost(sr: ServiceRequest): Fail
  def failOrUseHost[M[_], T](sr: ServiceRequest)(fn: String => M[T])(implicit monad: MonadCanFailWithException[M, Fail]): M[T] = sr.header("host") match {
    case Some(host) => fn(host)
    case None => monad.fail(failNoHost(sr))
  }
}

trait EditEntityRequestFailer[Fail] extends NoHostFailer[Fail] {
  def failNoJson(sr: ServiceRequest): Fail
  def failIdDoesntMatch(id: String, sr: ServiceRequest): Fail
  def failOrUseBody[M[_], T](sr: ServiceRequest)(fn: String => M[T])(implicit monad: MonadCanFailWithException[M, Fail]): M[T] =
    sr.body match {
    case Some(Body(body)) => fn(body)
    case None => monad.fail(failNoJson(sr))
  }


}

object EditEntityRequestFailer {
  implicit object EditEntityRequestFailerForThrowable extends EditEntityRequestFailer[Throwable] {
    override def failNoJson(sr: ServiceRequest): Throwable = EditEntityException(s"No json in the request\n$sr")
    override def failNoHost(sr: ServiceRequest): Throwable = EditEntityException(s"No host in the request\n$sr")
    override def failIdDoesntMatch(id: String, sr: ServiceRequest): Throwable = EditEntityException(s"Id mismatch in url $id\n$sr")
  }
}