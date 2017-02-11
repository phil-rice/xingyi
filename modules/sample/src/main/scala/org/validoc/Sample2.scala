package org.validoc

import org.validoc.domain._
import org.validoc.utils.aggregate.{Enricher, HasChildren}
import org.validoc.utils.concurrency.Async
import org.validoc.utils.http.{ServiceRequest, ToRequest, ToServiceResponse}
import org.validoc.utils.service.ServiceBuilder
import org.validoc.utils.time.NanoTimeService

import scala.concurrent.duration._


abstract class Sample2[M[_], HttpReq, HttpRes](mostPopularHttp: Service[M, HttpReq, HttpRes],
                                               promotionHttp: Service[M, HttpReq, HttpRes],
                                               programmeAndProductionHttp: Service[M, HttpReq, HttpRes])
                                              (implicit async: Async[M], toServiceResponse: ToServiceResponse[HttpRes],
                                               toHttpReq: ServiceRequest => HttpReq,
                                               timeService: NanoTimeService)
  extends ServiceBuilder[M, HttpReq, HttpRes] {


  val homePageService1 =
    aggregate(
      aggregate(
        (parse[MostPopularQuery, MostPopular] andThen cache(20, 2 minutes, 10 hours)) (mostPopularHttp),
        (parse[ProgrammeId, Programme] andThen cache(2000, 2 minutes, 10 hours)) (programmeAndProductionHttp)).
        enrich[EnrichedMostPopular],
      aggregate(
        (parse[HomePageQuery, Promotion] andThen cache(10, 2 minutes, 10 hours) andThen profile) (promotionHttp),
        (parse[ProductionId, Production] andThen cache(100, 2 minutes, 10 hours)) (programmeAndProductionHttp)).
        enrich[EnrichedPromotion])
      .merge[HomePageQuery, HomePage](HomePage.apply)// andThen cache[HomePageQuery, HomePage]((20, 2 minutes, 10 hours))

}