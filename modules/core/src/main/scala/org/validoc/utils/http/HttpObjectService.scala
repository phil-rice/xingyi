package org.validoc.utils.http

import org.validoc.utils.Service
import org.validoc.utils.concurrency.Async

import scala.language.higherKinds
import org.validoc.utils._
import org.validoc.utils.containers.MonadCanFail

class HttpObjectService[M[_], Fail, HttpReq, Req, HttpRes, Res](name: String,
                                                                rawClient: Service[M, HttpReq, HttpRes])
                                                               (implicit async: MonadCanFail[M, Fail],
                                                                toServiceResponse: ToServiceResponse[HttpRes],
                                                                toRequest: ToServiceRequest[Req],
                                                                responseCategoriser: ResponseCategoriser[Req],
                                                                responseProcessor: ResponseProcessor[M, Fail, Req, Res],
                                                                parser: ServiceResponse => Res,
                                                                fromServiceRequest: FromServiceRequest[HttpReq]) extends Service[M, Req, Res] {
  override def apply(req: Req) =
    (toRequest ~> fromServiceRequest ~> rawClient |=> toServiceResponse |+> responseCategoriser |==> responseProcessor) (req)
}
