package org.validoc.utils.http

import org.validoc.utils.Service
import org.validoc.utils.concurrency.Async
import org.validoc.utils.parser.ParserFinder
import org.validoc.utils.service.{MakeServiceMakerForClass, ServiceComposition}

import scala.language.higherKinds


trait HttpObjectServiceLanguage[M[_], HttpReq, HttpRes] extends ServiceComposition[M] {
  def asObject[Req: ToServiceRequest, Res: ParserFinder]
  (implicit async: Async[M],
   toServiceResponse: ToServiceResponse[HttpRes],
   fromServiceRequest: FromServiceRequest[HttpReq]) =
    serviceDescription2[ HttpReq, HttpRes, Req, Res, HttpObjectService[M, HttpReq, Req, HttpRes, Res]] { delegate => new HttpObjectService[M, HttpReq, Req, HttpRes, Res]("someName", delegate, ResponseProcessor.parsed[Req, Res]) }
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
