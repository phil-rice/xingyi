package org.validoc.sample

import org.validoc.language._
import org.validoc.sample.domain._
import org.validoc.utils._
import org.validoc.utils.http._
import org.validoc.utils.metrics.NullPutMetrics
import org.validoc.utils.service.ServerContext
import org.validoc.utils.success.SucceededFromFn
import org.validoc.utils.time.SystemClockNanoTimeService
import scala.language.higherKinds
import scala.language.postfixOps
import scala.concurrent.duration._
import scala.language.postfixOps


class PromotionSetup[Tag[M[_], _, _], M[_], HttpReq, HttpRes](s: IHttpSetup[Tag, M, HttpReq, HttpRes])(implicit
                                                                                                       makeHttpService: MakeHttpService[M, HttpReq, HttpRes],
                                                                                                       serverContext: ServerContext[HttpReq, HttpRes]) {

  type Setup = IHttpSetup[Tag, M, HttpReq, HttpRes]

  val mostPopularHttp: Tag[M, HttpReq, HttpRes] = s.rawService("mostPopular")

  val promotionHttp: Tag[M, HttpReq, HttpRes] = s.rawService("promotion")

  val programmeAndProductionsHttp: Tag[M, HttpReq, HttpRes] = s.rawService("programmeAndProductions")

  val enrichedMostPopularService: Tag[M, MostPopularQuery, EnrichedMostPopular] = {
    import s._
    aggregate(
      getCachedProfiledObject[MostPopularQuery, MostPopular]("client.mostPopular", 2 minutes, 10 hours, 20, mostPopularHttp),
      getCachedProfiledObject[ProgrammeId, Programme]("client.programme", 2 minutes, 10 hours, 2000, programmeAndProductionsHttp)).
      enrich[EnrichedMostPopular]
  }

  val enrichedPromotionService: Tag[M, PromotionQuery, EnrichedPromotion] = {
    import org.validoc.utils.functions.Functions._
    import s._
    aggregate(
      (httpCallout[PromotionQuery, Promotion] _ ~> profiled[PromotionQuery, Promotion] ~> cached(2 minutes, 10 hours, 20)) (promotionHttp),
      getCachedProfiledObject[ProductionId, Production]("client.production", 2 minutes, 10 hours, 2000, programmeAndProductionsHttp)).
      enrich[EnrichedPromotion]
  }

  val homePageService: Tag[M, ServiceRequest, ServiceResponse] = {
    import s._
    endpoint0("/endpoint")(
      aggregate(
        enrichedMostPopularService,
        enrichedPromotionService).
        merge[HomePageQuery, HomePage](HomePage.apply))
  }
}


object Sample3 extends App  {
  val setup = new PromotionSetup[StringServiceTag, Option, String, String]( ToStringInterpreter())

  import setup._

  println(enrichedMostPopularService)
  println
  println(homePageService)
  println
  println(enrichedPromotionService)
}
