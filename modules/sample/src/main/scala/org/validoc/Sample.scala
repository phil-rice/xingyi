package org.validoc

import org.validoc.domain._
import org.validoc.utils.concurrency.Async
import org.validoc.utils.concurrency.Async._
import org.validoc.utils.http.HttpObjectService
import org.validoc.utils.monads.Kleisli._


class Sample1[M[_] : Async, HttpReq, HttpRes](mostPopularHttp: Service[M, HttpReq, HttpRes],
                                              promotionHttp: Service[M, HttpReq, HttpRes],
                                              programmeHttp: Service[M, HttpReq, HttpRes],
                                              productionHttp: Service[M, HttpReq, HttpRes]) {

  implicit class ServicePimper[Req, Res](service: Service[M, Req, Res]) {
    def cached(config: String): Service[M, Req, Res] = ???

    def profiled: Service[M, Req, Res] = ???

    def aggregate[ReqM, ResM, Req2, Res2](service1: Service[M, Req2, Res2]): Service[M, ReqM, ResM] = ???
  }

  implicit class HttpServicePimper(service: Service[M, HttpReq, HttpRes]) {
    def parsed[Req, Res](parser: String): Service[M, Req,  Res] = ???
  }


  type Wrapped[M[_], Req, Res] = Service[M, Req, Res] => Service[M, Req, Res]

  def cached[Req, Res](config: String): Wrapped[M, Req, Res] = ???

  def profiled[Req, Res](config: String): Wrapped[M, Req, Res] = ???

  def client[Req, Res](parser: String): Service[M, HttpReq, HttpRes] => Service[M, Req, Res] = ???


  val mostPopularService: MostPopularService[M] =
    mostPopularHttp.parsed[Unit, MostPopular]("someParser").cached("someConfig").profiled

  val productionService: ProductionService[M] =
    productionHttp.parsed[ProductionId, Production]("someParser").profiled

  val programmeService: ProgrammeService[M] =
    programmeHttp.parsed[ProgrammeId, Programme]("someParser").profiled

  val promotionService: PromotionService[M] =
    promotionHttp.parsed[Unit, List[Promotion]]("someParser").cached("someConfig").profiled

  val enrichMostPopularService: EnrichMostPopularService[M] =
    new ServicePimper[Unit, MostPopular](mostPopularService).aggregate(productionService)


}

