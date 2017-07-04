package org.validoc.sample

import scala.language.{higherKinds, postfixOps}
//
//abstract class Sample2[M[_] : Async, HttpReq, HttpRes](mostPopularHttp: Service[M, HttpReq, HttpRes],
//                                                       promotionHttp: Service[M, HttpReq, HttpRes],
//                                                       programmeAndProductionHttp: Service[M, HttpReq, HttpRes])
//                                                      (implicit toServiceResponse: ToServiceResponse[HttpRes],
//                                                       fromServiceRequest: FromServiceRequest[HttpReq],
//                                                       timeService: NanoTimeService
//                                                      )
//  extends ServiceBuilder[M, HttpReq, HttpRes] {
//
//  implicit val fromJsonPromotion: FromJson[Promotion] = ???
//  implicit val fromJsonProduction: FromJson[Production] = ???
//  implicit val fromJsonProgramme: FromJson[Programme] = ???
//  implicit val fromJsonmostPopular: FromJson[MostPopular] = ???
//
//  def parseCacheAndProfile[Req: ToServiceRequest : CachableKey, Res: ParserFinder : CachableResult](maxCacheSize: Int, timeToStale: Duration, timeToDead: Duration): (Service[M, HttpReq, HttpRes]) => Service[M, Req, Res] =
//    (parse[Req, Res] andThen cache(maxCacheSize, timeToStale, timeToDead) andThen profile)
//
//  //  val x = mostPopularHttp[MostPopular] ~~~> pipe
//
//
//  val mostPopularService: Service[M, MostPopularQuery, EnrichedMostPopular] =
//    aggregate(
//      mostPopularHttp ~~~> parseCacheAndProfile[MostPopularQuery, MostPopular](20, 2 minutes, 10 hours),
//      programmeAndProductionHttp ~~~> parseCacheAndProfile[ProgrammeId, Programme](2000, 2 minutes, 10 hours)).
//      enrich[EnrichedMostPopular]
//
//  private val promotionService = aggregate(
//    (parse[HomePageQuery, Promotion] andThen cache(10, 2 minutes, 10 hours) andThen profile) (promotionHttp),
//    (parse[ProductionId, Production] andThen cache(100, 2 minutes, 10 hours)) (programmeAndProductionHttp)).
//    enrich[EnrichedPromotion]
//
//  val homePageService1 = aggregate(mostPopularService, promotionService).merge[HomePageQuery, HomePage](HomePage.apply)
//
//}