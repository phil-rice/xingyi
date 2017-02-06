package org.validoc.utils.http

import org.validoc.utils.Service
import org.validoc.utils.concurrency.Async

import scala.util.{Failure, Success}


class HttpObjectService[M[_] : Async, HttpReq, Req, HttpRes, Res](name: String,
                                                                  rawClient: Service[M, HttpReq, HttpRes],
                                                                  responseProcessor: ResponseProcessor[Req, Res])
                                                                 (implicit toRequest: ToHttpRequest[Req, HttpReq],
                                                                  toServiceResponse: ToServiceResponse[HttpRes]) extends Service[M, Req, Res] {

  import Async._

  def requestDetails(req: Req) = RequestDetails(req, s"Calling $name with ${toRequest.toSummary(req)}")

  override def apply(req: Req): M[Res] = {
    val httpReq = toRequest.toHttpRequest(req)
    rawClient(httpReq).transform[Res] { tryRes =>
      tryRes match {
        case Success(httpRes) => {
          val serviceResponse = toServiceResponse.response(httpRes)
          serviceResponse.status match {
            case Status.Ok => responseProcessor.statusOk(serviceResponse)
            case Status.NotFound => responseProcessor.statusNotFound(requestDetails(req), serviceResponse)
            case _ => responseProcessor.statusUnexpected(requestDetails(req), serviceResponse)
          }
        }.lift
        case Failure(t) => responseProcessor.exception(requestDetails(req), t).lift
      }
    }
  }
}
