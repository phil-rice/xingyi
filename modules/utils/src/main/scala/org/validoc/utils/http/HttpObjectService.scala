package org.validoc.utils.http

import org.validoc.utils.Service
import org.validoc.utils.concurrency.Async

import scala.util.{Failure, Success}


class HttpObjectService[M[_] : Async, HttpReq, Req: ToRequest, HttpRes, Res](name: String,
                                                                             rawClient: Service[M, HttpReq, HttpRes],
                                                                             responseProcessor: ResponseProcessor[Req, Res])
                                                                            (implicit toServiceResponse: ToServiceResponse[HttpRes],
                                                                             toHttpReq: ServiceRequest => HttpReq) extends Service[M, Req, Res] {

  import Async._

  val toRequest = implicitly[ToRequest[Req]]


  def requestDetails(req: Req) = RequestDetails(req, s"Calling $name with $req")
  override def apply(req: Req): M[Res] = {
    val request: ServiceRequest = toRequest.toRequest(req)
    val httpReq = toHttpReq(request)
    rawClient(httpReq).transform[Res] {
      tryRes =>

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
