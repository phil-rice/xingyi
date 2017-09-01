package org.validoc.utils.http

import org.validoc.utils.Service
import org.validoc.utils.concurrency.Async
import org.validoc.utils.serviceTree.{HttpReqHttpResServiceLanguageExtension, ServiceLanguageExtension}

import scala.language.higherKinds
import scala.reflect.ClassTag


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


trait HttpObjectServiceLanguageExtension[M[_], HttpReq, HttpRes] extends ServiceLanguageExtension[M] {
  def objectify[Req: ClassTag, Res: ClassTag]
  (name: String, responseProcessor: ResponseProcessor[Req, Res])
  (implicit toServiceResponse: ToServiceResponse[HttpRes],
   toRequest: ToServiceRequest[Req],
   fromServiceRequest: FromServiceRequest[HttpReq],
   httpReqClassTag: ClassTag[HttpReq],
   httpResClassTag: ClassTag[HttpRes]): ServiceTransformer[HttpReq, HttpRes, Req, Res] = {
    childTree =>
      transform[HttpReq, HttpRes, Req, Res](s"GenericCustomClient", childTree, delegate => new HttpObjectService[M, HttpReq, Req, HttpRes, Res](name, delegate, responseProcessor))
  }
}

