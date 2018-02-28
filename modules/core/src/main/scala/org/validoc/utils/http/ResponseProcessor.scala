package org.validoc.utils.http

import org.validoc.utils.parser.Parser

import scala.annotation.implicitNotFound
import scala.language.higherKinds

@implicitNotFound("""Cannot find ResponseProcessor[${Fail},${Req},${Res}] The easiest way is to have a ResponseParser and a Failer in scope
     To check this you can try
        val failer = implicitly[Failer[${Fail}]]
        val responseParser = implicitly[ResponseParser[${Fail},${Req},${Res}]]
        the compiler should give you a more helpful message then
  """)
trait ResponseProcessor[Fail, Req, Res] extends (ResponseState[Req] => Either[Fail, Res])

trait ResponseParser[Fail, Req, Res] extends (Req => String => Either[Fail, Res])

object ResponseParser {
  def defaultDirtyParser[Fail, Req, Res](implicit parser: Parser[Res]) = new ResponseParser[Fail, Req, Res] {
    override def apply(req: Req) = { string => Right(parser(string)) }
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

  implicit object FailerForVoid extends Failer[Void] {
    override def notFound[Req](req: Req, response: ServiceResponse) = ???
    override def unexpected[Req](req: Req, response: ServiceResponse) = ???
    override def exception[Req](req: Req, throwable: Throwable) = ???
    override def idNotFind(serviceRequest: ServiceRequest): Void = ???
    override def pathNotFound(serviceRequest: ServiceRequest): Void = ???
  }

  implicit object FailerForThrowable extends Failer[Throwable] {
    override def notFound[Req](req: Req, response: ServiceResponse) = ???
    override def unexpected[Req](req: Req, response: ServiceResponse) = ???
    override def exception[Req](req: Req, throwable: Throwable) = ???
    override def idNotFind(serviceRequest: ServiceRequest): Throwable = ???
    override def pathNotFound(serviceRequest: ServiceRequest): Throwable = ???
  }

}

object ResponseProcessor {

  implicit def defaultResponseProcessor[Fail, Req, Res](implicit parser: ResponseParser[Fail, Req, Res], failer: Failer[Fail]) = new ResponseProcessor[Fail, Req, Res] {
    override def apply(state: ResponseState[Req]) = {
      state match {
        case ResponseOk(req, response) => parser(req)(response.body.s)
        case ResponseNotFound(req, response) => Left(failer.notFound(req, response))
        case ResponseUnexpectedStatusCode(req, response) => Left(failer.unexpected(req, response))
        case ResponseException(req, throwable) => Left(failer.exception(req, throwable))
      }
    }
  }
}
