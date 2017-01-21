package org.validoc.utils.http

import org.validoc.utils.Futurable
import org.validoc.utils.service.{ServiceDisplayData, ServiceWithDisplayData, WrappingService}


class HttpClientService[M[_] : Futurable, HttpReq, Req, HttpRes, Res](name: String, rawClient: HttpReq => M[HttpRes], responseProcessor: ResponseProcessor[Req, Res], priority: Int = 0)
                                                                     (implicit toRequest: ToHttpRequest[Req, HttpReq],
                                                                      toServiceResponse: ToServiceResponse[HttpRes])
  extends (Req => M[Res]) with ServiceWithDisplayData {

  import Futurable._

  override def apply(req: Req): M[Res] = {
    val httpReq = toRequest.toHttpRequest(req)
    rawClient(httpReq).map { httpRes: HttpRes =>
      val serviceResponse = toServiceResponse.response(httpRes)

      def requestDetails = RequestDetails(req, s"Calling ${toRequest.toSummary(req)}")

      serviceResponse.status match {
        case Status.Ok => responseProcessor.statusOk(serviceResponse)
        case Status.NotFound => responseProcessor.statusNotFound(requestDetails, serviceResponse)
        case _ => responseProcessor.statusUnexpected(requestDetails, serviceResponse)
      }
    }.onFailure { case e: Exception => responseProcessor.exception(e) }
  }

  override def displayData: ServiceDisplayData = ServiceDisplayData(getClass.getSimpleName, name, priority)

  override def status: Option[String] = ???
}
