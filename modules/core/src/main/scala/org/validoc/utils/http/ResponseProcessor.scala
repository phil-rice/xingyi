package org.validoc.utils.http

import org.validoc.utils.parser.Parser

import scala.language.higherKinds

/** This is usually made by having a parser and a failer in scope, and using the ResponseProcessor default. It can obviously be overridden */
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
}
object Failer {
  implicit object FailerForVoid extends Failer[Void] {
    override def notFound[Req](req: Req, response: ServiceResponse) = ???
    override def unexpected[Req](req: Req, response: ServiceResponse) = ???
    override def exception[Req](req: Req, throwable: Throwable) = ???
  }

  implicit object FailerForException extends Failer[Throwable] {
    override def notFound[Req](req: Req, response: ServiceResponse) = ???
    override def unexpected[Req](req: Req, response: ServiceResponse) = ???
    override def exception[Req](req: Req, throwable: Throwable) = ???
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
