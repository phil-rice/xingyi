package org.validoc.utils.http

import org.validoc.utils.concurrency.Async
import org.validoc.utils.logging.LoggingService
import org.validoc.utils.parser.ParserFinder
import org.validoc.utils.service.{MakeServiceMakerForClass, MakeServiceMakerForClassWithParam}
import org.validoc.utils.success.Succeeded
import org.validoc.utils.{FromServiceRequest, Service, ToServiceRequest, ToServiceResponse}

import scala.language.higherKinds

object HttpObjectService {
  implicit def makeHttpObjectService[OldService <: HttpReq => M[HttpRes], M[_] : Async, HttpReq: FromServiceRequest, HttpRes: ToServiceResponse, Req: ToServiceRequest, Res: ParserFinder] =
    new MakeServiceMakerForClass[OldService, HttpObjectService[M, HttpReq, Req, HttpRes, Res]] {
      override def apply(delegate: OldService): HttpObjectService[M, HttpReq, Req, HttpRes, Res] =
        new HttpObjectService[M, HttpReq, Req, HttpRes, Res]("someName", delegate, ResponseProcessor.parsed[Req, Res])
    }
}

class HttpObjectService[M[_] : Async, HttpReq, Req, HttpRes, Res](name: String,
                                                                  rawClient: Service[M, HttpReq, HttpRes],
                                                                  responseProcessor: ResponseProcessor[Req, Res])
                                                                 (implicit toServiceResponse: ToServiceResponse[HttpRes],
                                                                  toRequest: ToServiceRequest[Req],
                                                                  fromServiceRequest: FromServiceRequest[HttpReq]) extends Service[M, Req, Res] {

  import Async._
  import org.validoc.utils.functions.Functions._

  implicit def requestDetails = RequestDetails[Req](name) _

  def processServiceResponse(req: Req) = { serviceResponse: ServiceResponse =>
    serviceResponse.status match {
      case Status.Ok => responseProcessor.statusOk(serviceResponse)
      case Status.NotFound => responseProcessor.statusNotFound(requestDetails(req), serviceResponse)
      case _ => responseProcessor.statusUnexpected(requestDetails(req), serviceResponse)
    }
  }

  override def apply(req: Req): M[Res] = {
    (toRequest ~> fromServiceRequest ~> rawClient transformAndLift(
      responseProcessor.exception(req),
      toServiceResponse ~> processServiceResponse(req))
      ) (req)
  }

}
