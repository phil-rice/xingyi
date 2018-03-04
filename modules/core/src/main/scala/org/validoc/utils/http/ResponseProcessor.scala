package org.validoc.utils.http

import org.validoc.utils.functions.{Liftable, MonadCanFail}
import org.validoc.utils.parser.Parser

import scala.annotation.implicitNotFound
import scala.language.higherKinds
import org.validoc.utils._

trait ResponseParser[Fail, Req, Res] extends (RequestAndServiceResponse[Req] => Either[Fail, Res])

object ResponseParser {
  implicit def defaultDirtyParser[M[_], Fail, Req, Res](implicit parser: Parser[Res]) = new ResponseParser[Fail, Req, Res] {
    override def apply(req: RequestAndServiceResponse[Req]) = req match {
      case RequestAndServiceResponse(req, sr) => Right(parser(sr.body.s))
    }
  }
}

trait Failer[Fail] {
  def notFound[Req](req: Req, response: ServiceResponse): Fail
  def unexpected[Req](req: Req, response: ServiceResponse): Fail
  def exception[Req](req: Req, throwable: Throwable): Fail
  def idNotFind(serviceRequest: ServiceRequest): Fail
  def pathNotFound(serviceRequest: ServiceRequest): Fail
}

object Failer {


  implicit object FailerForThrowable extends Failer[Throwable] {
    override def notFound[Req](req: Req, response: ServiceResponse) = ???
    override def unexpected[Req](req: Req, response: ServiceResponse) = ???
    override def exception[Req](req: Req, throwable: Throwable) = ???
    override def idNotFind(serviceRequest: ServiceRequest): Throwable = ???
    override def pathNotFound(serviceRequest: ServiceRequest): Throwable = throw new RuntimeException(s"Path not found $serviceRequest")
  }

}

