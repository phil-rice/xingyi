package org.validoc.utils.http

import org.validoc.utils._
import org.validoc.utils.functions.MonadCanFail

import scala.language.higherKinds


class HttpObjectService[M[_], Fail, HttpReq, HttpRes](implicit async: MonadCanFail[M, Fail],
                                                      toServiceResponse: ToServiceResponse[HttpRes],
                                                      toHttpReq: FromServiceRequest[HttpReq]) {

  def objectify[Req, Res](rawClient: HttpReq => M[HttpRes])
                         (implicit toRequest: ToServiceRequest[Req], categoriser: ResponseCategoriser[Req], responseProcessor: ResponseProcessor[Fail, Req, Res]): (Req => M[Res]) =
    toRequest ~> toHttpReq ~> rawClient |=> toServiceResponse |+> categoriser |=|> responseProcessor
}
