package org.validoc

import org.validoc.domain._
import org.validoc.utils.aggregate.{Enricher, HasChildren}
import org.validoc.utils.concurrency.Async

trait WrappedTypes[M[_]] {

  type Wrapped[Req, Res] = Service[M, Req, Res] => Service[M, Req, Res]
  type Modify[Req1, Res1, Req2, Res2] = Service[M, Req1, Res1] => Service[M, Req2, Res2]
}


trait ServiceTransforms[M[_], HttpReq, HttpRes] extends WrappedTypes[M] {
  implicit val async: Async[M]

  def parserTransformer[Req, Res](parser: String): Modify[HttpReq, HttpRes, Req, Res] = ???

  def cacheTransformer[Req, Res](config: String): Wrapped[Req, Res] = ???

  def profileTransformer[Req, Res]: Wrapped[Req, Res] = ???

  def aggregate[P, C](serviceP: P, serviceC: C) = (serviceP, serviceC)

  implicit class Service2Pimper[Req1, Res1, Req2, Res2](tuple: (Req1 => M[Res1], Req2 => M[Res2])) {
    def enrich[ResE](implicit enricher: Enricher[ResE, Res1, Res2], children: HasChildren[Res1, Req2]): Service[M, Req1, ResE] = ???

    def merge[ReqE, ResE](merger: (Res1, Res2) => ResE)(implicit reqMtoReq1: ReqE => Req1, reqMtoReq2: ReqE => Req2): Service[M, Req1, ResE] = ???
  }

}

class Sample2[M[_], HttpReq, HttpRes](mostPopularHttp: Service[M, HttpReq, HttpRes],
                                      promotionHttp: Service[M, HttpReq, HttpRes],
                                      programmeHttp: Service[M, HttpReq, HttpRes],
                                      productionHttp: Service[M, HttpReq, HttpRes])(implicit val async: Async[M])
  extends WrappedTypes[M] with ServiceTransforms[M, HttpReq, HttpRes] {

  def parse[Req, Res](parser: String): Modify[HttpReq, HttpRes, Req, Res] = ???

  def cache[Req, Res](config: String): Wrapped[Req, Res] = ???

  def profile[Req, Res]: Wrapped[Req, Res] = ???

  def parseCacheAndProfile[Req, Res](parser: String, config: String) =
    parse[Req, Res](parser) andThen cache(config) andThen profile

  val homePageService1 =
    aggregate(
      aggregate(
        parseCacheAndProfile[Unit,MostPopular]("parser", "config")(mostPopularHttp),
        (parse[ProgrammeId, Programme]("parse") andThen cache("config")) (programmeHttp)).
        enrich[EnrichedMostPopular],
      aggregate(
        (parse[Unit, Promotion]("parser") andThen cache("config") andThen profile) (promotionHttp),
        (parse[ProductionId, Production]("parse") andThen cache("config")) (productionHttp)).
        enrich[EnrichedPromotion])
      .merge[Unit, HomePage](HomePage)

}