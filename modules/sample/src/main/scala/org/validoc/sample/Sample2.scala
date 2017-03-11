package org.validoc.sample

import org.validoc.sample.domain._
import org.validoc.utils.caching.{CachableKey, CachableResult}
import org.validoc.utils.concurrency.Async
import org.validoc.utils.functions.Functions._
import org.validoc.utils.parser.ParserFinder
import org.validoc.utils.service.ServiceBuilder
import org.validoc.utils.time.NanoTimeService
import org.validoc.utils.{FromServiceRequest, ToServiceRequest, ToServiceResponse}

import scala.concurrent.duration._
import scala.language.{higherKinds, postfixOps}
abstract class Sample2[M[_] : Async, HttpReq, HttpRes](mostPopularHttp: Service[M, HttpReq, HttpRes],
                                                       promotionHttp: Service[M, HttpReq, HttpRes],
                                                       programmeAndProductionHttp: Service[M, HttpReq, HttpRes])
                                                      (implicit toServiceResponse: ToServiceResponse[HttpRes],
                                                       fromServiceRequest: FromServiceRequest[HttpReq],
                                                       timeService: NanoTimeService)
  extends ServiceBuilder[M, HttpReq, HttpRes] {

  def parseCacheAndProfile[Req: ToServiceRequest : CachableKey, Res: ParserFinder : CachableResult](maxCacheSize: Int, timeToStale: Duration, timeToDead: Duration): (Service[M, HttpReq, HttpRes]) => Service[M, Req, Res] =
    (parse[Req, Res] andThen cache(maxCacheSize, timeToStale, timeToDead) andThen profile)

  //  val x = mostPopularHttp[MostPopular] ~~~> pipe


  val mostPopularService: Service[M, MostPopularQuery, EnrichedMostPopular] =
    aggregate(
      mostPopularHttp ~~~> parseCacheAndProfile[MostPopularQuery, MostPopular](20, 2 minutes, 10 hours),
      programmeAndProductionHttp ~~~> parseCacheAndProfile[ProgrammeId, Programme](2000, 2 minutes, 10 hours)).
      enrich[EnrichedMostPopular]

  private val promotionService = aggregate(
    (parse[HomePageQuery, Promotion] andThen cache(10, 2 minutes, 10 hours) andThen profile) (promotionHttp),
    (parse[ProductionId, Production] andThen cache(100, 2 minutes, 10 hours)) (programmeAndProductionHttp)).
    enrich[EnrichedPromotion]

  val homePageService1 = aggregate(mostPopularService, promotionService).merge[HomePageQuery, HomePage](HomePage.apply)

}