package org.validoc.utils.http

import org.validoc.utils.Service
import org.validoc.utils.concurrency.{Futurable, FuturableWithFailure}
import org.validoc.utils.service.{ServiceDisplayData, ServiceWithDisplayData}


class HttpObjectService[M[_], F, HttpReq, Req, HttpRes, Res](name: String, rawClient: HttpReq => M[HttpRes], responseProcessor: ResponseProcessor[Req, Res, F], priority: Int = 0)
                                                            (implicit futurableWithFailure: FuturableWithFailure[M, F],
                                                             toRequest: ToHttpRequest[Req, HttpReq],
                                                             toServiceResponse: ToServiceResponse[HttpRes])
  extends Service[M,Req,Res] with ServiceWithDisplayData {

  import Futurable._

  override def apply(req: Req): M[Res] = {
    val httpReq = toRequest.toHttpRequest(req)
    val onSuccess = { httpRes: HttpRes =>
      val serviceResponse = toServiceResponse.response(httpRes)

      def requestDetails = RequestDetails(req, s"Calling ${toRequest.toSummary(req)}")

      implicitly[Futurable[M]].lift(serviceResponse.status match {
        case Status.Ok => responseProcessor.statusOk(serviceResponse)
        case Status.NotFound => responseProcessor.statusNotFound(requestDetails, serviceResponse)
        case _ => responseProcessor.statusUnexpected(requestDetails, serviceResponse)
      })
    }
    val onFailure: (F) => M[Res] = { e: F => implicitly[FuturableWithFailure[M, F]].liftFailure[Res](responseProcessor.exception(e)) }
    rawClient(httpReq).onComplete(onSuccess, onFailure)
  }

  override def displayData: ServiceDisplayData = ServiceDisplayData(getClass.getSimpleName, name, priority)

  override def status: Option[String] = ???
}
