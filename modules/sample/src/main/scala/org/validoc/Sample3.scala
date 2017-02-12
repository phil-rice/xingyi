package org.validoc

import org.validoc.domain._
import org.validoc.utils.caching.{CachableKey, CachableResult}
import org.validoc.utils.http.{ToServiceRequest, _}
import org.validoc.utils.parser.ParserFinder
import org.validoc.utils.service.{IHttpSetup, IService, ServiceInterpreters, ServiceTag}
import org.validoc.utils.time.{NanoTimeService, SystemClockNanoTimeService}

import scala.concurrent.duration._
import scala.reflect.ClassTag


trait BruceSetup {

  def mostPopularHttp[T, HttpReq, HttpRes](implicit s: IHttpSetup[T, HttpReq, HttpRes]): ServiceTag[T, HttpReq, HttpRes] = s.rawService("mostPopular")

  def promotionHttp[T, HttpReq, HttpRes](implicit s: IHttpSetup[T, HttpReq, HttpRes]): ServiceTag[T, HttpReq, HttpRes] = s.rawService("promotion")

  def programmeAndProductionsHttp[T, HttpReq, HttpRes](implicit s: IHttpSetup[T, HttpReq, HttpRes]): ServiceTag[T, HttpReq, HttpRes] = s.rawService("programmeAndProductions")

  def getObject[T, HttpReq, HttpRes: ToServiceResponse, Req: ClassTag : ToServiceRequest : CachableKey, Res: ParserFinder : ClassTag : CachableResult]
  (timeToStale: Duration, timeToDead: Duration, maxSize: Int, rawService: ServiceTag[T, HttpReq, HttpRes])
  (implicit s: IHttpSetup[T, HttpReq, HttpRes], toHttpReq: (ServiceRequest) => HttpReq, nanoTimeService: NanoTimeService): ServiceTag[T, Req, Res] = {
    s.cached[Req, Res](timeToStale, timeToDead, maxSize, s.profiled(s.httpCallout(rawService)))
  }

  def enrichedMostPopularService[T, HttpReq, HttpRes: ToServiceResponse](implicit s: IHttpSetup[T, HttpReq, HttpRes], toHttpReq: (ServiceRequest) => HttpReq, nanoTimeService: NanoTimeService): ServiceTag[T, MostPopularQuery, EnrichedMostPopular] =
    s.enrich[MostPopularQuery, MostPopular, EnrichedMostPopular, ProgrammeId, Programme](
      getObject[T, HttpReq, HttpRes, MostPopularQuery, MostPopular](2 minutes, 10 hours, 20, mostPopularHttp),
      getObject[T, HttpReq, HttpRes, ProgrammeId, Programme](2 minutes, 10 hours, 2000, programmeAndProductionsHttp), //
      { mp: MostPopular => mp.programmeIds }, //
      { (mp: MostPopular, programmes: Seq[Programme]) => EnrichedMostPopular(mp, programmes) }
    )

  def enrichedPromotionService[T, HttpReq, HttpRes: ToServiceResponse](implicit s: IHttpSetup[T, HttpReq, HttpRes], toHttpReq: (ServiceRequest) => HttpReq, nanoTimeService: NanoTimeService) =
    s.enrich[HomePageQuery, Promotion, EnrichedPromotion, ProductionId, Production](
      getObject[T, HttpReq, HttpRes, HomePageQuery, Promotion](2 minutes, 10 hours, 20, promotionHttp),
      getObject[T, HttpReq, HttpRes, ProductionId, Production](2 minutes, 10 hours, 2000, programmeAndProductionsHttp), //
      { p: Promotion => p.productionIds }, //
      { (p: Promotion, productions: Seq[Production]) => EnrichedPromotion(p.name, productions) }
    )

  def homePageService[T, HttpReq, HttpRes: ToServiceResponse](implicit s: IHttpSetup[T, HttpReq, HttpRes], toHttpReq: (ServiceRequest) => HttpReq, nanoTimeService: NanoTimeService) =
    s.merge[HomePageQuery, HomePage, MostPopularQuery, EnrichedMostPopular, HomePageQuery, EnrichedPromotion](
      enrichedMostPopularService,
      enrichedPromotionService,
      x => MostPopularQuery,
      x => x,
      HomePage.apply
    )
}

class BruceSetup2[T, HttpReq, HttpRes: ToServiceResponse](implicit s: IHttpSetup[T, HttpReq, HttpRes], toHttpReq: (ServiceRequest) => HttpReq, nanoTimeService: NanoTimeService) {

  def mostPopularHttp: ServiceTag[T, HttpReq, HttpRes] = s.rawService("mostPopular")

  def promotionHttp: ServiceTag[T, HttpReq, HttpRes] = s.rawService("promotion")

  def programmeAndProductionsHttp: ServiceTag[T, HttpReq, HttpRes] = s.rawService("programmeAndProductions")

  def getObject[Req: ClassTag : ToServiceRequest : CachableKey, Res: ParserFinder : ClassTag : CachableResult]
  (timeToStale: Duration, timeToDead: Duration, maxSize: Int, rawService: ServiceTag[T, HttpReq, HttpRes]): ServiceTag[T, Req, Res] =
    s.cached[Req, Res](timeToStale, timeToDead, maxSize, s.profiled(s.httpCallout(rawService)))


  def enrichedMostPopularService: ServiceTag[T, MostPopularQuery, EnrichedMostPopular] =
    s.enrich[MostPopularQuery, MostPopular, EnrichedMostPopular, ProgrammeId, Programme](
      getObject[MostPopularQuery, MostPopular](2 minutes, 10 hours, 20, mostPopularHttp),
      getObject[ProgrammeId, Programme](2 minutes, 10 hours, 2000, programmeAndProductionsHttp), //
      { mp: MostPopular => mp.programmeIds }, //
      { (mp: MostPopular, programmes: Seq[Programme]) => EnrichedMostPopular(mp, programmes) }
    )

  def enrichedPromotionService =
    s.enrich[HomePageQuery, Promotion, EnrichedPromotion, ProductionId, Production](
      getObject[HomePageQuery, Promotion](2 minutes, 10 hours, 20, promotionHttp),
      getObject[ProductionId, Production](2 minutes, 10 hours, 2000, programmeAndProductionsHttp), //
      { p: Promotion => p.productionIds }, //
      { (p: Promotion, productions: Seq[Production]) => EnrichedPromotion(p.name, productions) }
    )

  def homePageService =
    s.merge[HomePageQuery, HomePage, MostPopularQuery, EnrichedMostPopular, HomePageQuery, EnrichedPromotion](
      enrichedMostPopularService,
      enrichedPromotionService,
      x => MostPopularQuery,
      x => x,
      HomePage.apply
    )
}


object Sample3 extends BruceSetup with App {

  implicit object ServiceResponseForString extends ToServiceResponse[String] {
    override def apply(v1: String): ServiceResponse = ServiceResponse(Status.Ok, Body(v1), ContentType("text/plain"))
  }

  implicit object StringToServiceRequest extends (String => ServiceRequest) {
    override def apply(v1: String): ServiceRequest = ServiceRequest(Get, Uri(v1))
  }

  implicit object ServiceRequestToString extends (ServiceRequest => String) {
    override def apply(v1: ServiceRequest): String = v1.uri.asUriString
  }


  implicit val printer = new ServiceInterpreters.ServiceToString[String, String]

  implicit val nanoTimeService = SystemClockNanoTimeService

  println(homePageService[String, String, String])
  println
  println(enrichedPromotionService[String, String, String])
}
