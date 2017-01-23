package org.validoc.utils.serviceBuilder

import org.validoc.utils.concurrency.FuturableWithFailure
import org.validoc.utils.http.{HttpObjectService, ResponseProcessor, ToHttpRequest, ToServiceResponse}

trait ConcurencyLanguage {
  type M[T]
  type Service[M[_], Req, Res] = (Req => M[Res])

}

trait HttpLanguage extends ConcurencyLanguage {
  type HttpReq
  type HttpRes
  type ToHttpReq[Req] = ToHttpRequest[Req, HttpReq]
  type HttpService = Service[M, HttpReq, HttpRes]
  type HttpF

  implicit def futurableWithFailureForHttp: FuturableWithFailure[M, HttpF]
}

trait ServiceBuilderLanguage extends HttpLanguage {

  implicit def toServiceResponse: ToServiceResponse[HttpRes]

  implicit class HttpServicePimper(rawService: HttpService) {
    def objectService[Req: ToHttpReq, Res](name: String, responseProcessor: ResponseProcessor[Req, Res, HttpF]) =
      new HttpObjectService[M, HttpF, HttpReq, Req, HttpRes, Res](name, rawService, responseProcessor)
  }

  def rawHttpClient: Service[M, HttpReq, HttpRes]

}

