package one.xingyi.utils.http

import one.xingyi.utils.functions.MonadCanFail
import one.xingyi.utils.language.Language._

import scala.language.higherKinds
//We are going to lift the service response into the Monad.  So it might 'fail' or throw an exception depending on the monad chosen
//the request is needed for validation. For example the request might have a security token and that has to be related to the response

case class RequestAndServiceResponse[Req](req: Req, serviceResponse: ServiceResponse)

trait ResponseCategoriser[Req] {
  def categorise[Fail](implicit failer: Failer[Fail]): Req => ServiceResponse => Either[Fail, RequestAndServiceResponse[Req]]
}

object ResponseCategoriser {
  def apply[Req]: ResponseCategoriser[Req] = new ResponseCategoriser[Req] {
    override def categorise[Fail](implicit failer: Failer[Fail]): Req => ServiceResponse => Either[Fail, RequestAndServiceResponse[Req]] =
      req => serviceResponse =>
        serviceResponse.status.code match {
          case x if x / 100 == 2 => Right(RequestAndServiceResponse(req, serviceResponse))
          case 404 => Left(failer.notFound(req, serviceResponse))
          case _ => Left(failer.unexpected(req, serviceResponse))
        }

  }

  implicit def default[Req] = apply[Req]

}
