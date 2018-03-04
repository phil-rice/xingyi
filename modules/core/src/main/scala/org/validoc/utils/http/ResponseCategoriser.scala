package org.validoc.utils.http

import org.validoc.utils.functions.MonadCanFail

import scala.annotation.implicitNotFound
import org.validoc.utils._
import scala.language.higherKinds
//We are going to lift the service response into the Monad.  So it might 'fail' or throw an exception depending on the monad chosen
//the request is needed for validation. For example the request might have a security token and that has to be related to the response

case class RequestAndServiceResponse[Req](req: Req, serviceResponse: ServiceResponse)

trait ResponseCategoriser[M[_], Req] extends (Req => ServiceResponse => M[RequestAndServiceResponse[Req]])

object ResponseCategoriser {
  def apply[M[_], Fail, Req](implicit monadCanFail: MonadCanFail[M, Fail], failer: Failer[Fail]): ResponseCategoriser[M, Req] = new ResponseCategoriser[M, Req] {
    override def apply(req: Req) = { serviceResponse =>
      serviceResponse.status.code match {
        case x if x / 100 == 2 => RequestAndServiceResponse(req, serviceResponse).liftM
        case 404 => failer.notFound(req, serviceResponse).fail
        case _ => failer.unexpected(req, serviceResponse).fail
      }
    }
  }

  implicit def default[M[_], Fail, Req](implicit monadCanFail: MonadCanFail[M, Fail], failer: Failer[Fail]) = apply[M, Fail, Req]

}
