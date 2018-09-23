package one.xingyi.core.client

import one.xingyi.core.http.ServiceResponse
import one.xingyi.core.json.FromJson
import one.xingyi.core.monad.MonadWithException

import scala.language.higherKinds

case class UnexpectedResponse(sr: ServiceResponse) extends Exception(sr.toString)
case class UrlDidntResponse(uri: String, cause: Throwable) extends Exception(uri, cause)

trait ResponseProcessor[M[_], Req, Res] extends (Req => ServiceResponse => M[Res])

abstract class AbstractResponseProcess[M[_], Req, Res](implicit monad: MonadWithException[M]) extends ResponseProcessor[M, Req, Res] {
  override def apply(req: Req): ServiceResponse => M[Res] = sr => sr.status.code / 100 match {
    case 2 => process200(req, sr)
    case _ => processOther(req, sr)
  }
  def process200(req: Req, sr: ServiceResponse): M[Res]
  def processOther(req: Req, sr: ServiceResponse): M[Res] = monad.exception(UnexpectedResponse(sr))
}
object ResponseProcessor {
  implicit def default[M[_], Req, Res](implicit monad: MonadWithException[M], fromJson: FromJson[Res]): ResponseProcessor[M, Req, Res] = new AbstractResponseProcess[M, Req, Res] {
    override def process200(req: Req, sr: ServiceResponse): M[Res] = monad.liftM(fromJson(sr.body.s))
  }
}