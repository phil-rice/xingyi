package org.validoc.sample

import org.validoc.sample.domain._
import org.validoc.utils.caching.{CachableKey, CachableResult, DurationStaleCacheStategy}
import org.validoc.utils.concurrency.Async
import org.validoc.utils.http._
import org.validoc.utils.json.{FromJson, ToJson}
import org.validoc.utils.map.MaxMapSizeStrategy
import org.validoc.utils.serviceTree.HttpReqHttpResServiceLanguageExtension
import org.validoc.utils.time.NanoTimeService

import scala.language.{higherKinds, postfixOps}
import scala.reflect.ClassTag


trait PromotionServiceNames {
  val mostPopularServiceName = ServiceName("mostPopular")
  val promotionServiceName = ServiceName("promotion")
  val programmeAndProductionServiceName = ServiceName("programmeAndProduction")
}

class PromotionSetup[M[_], HttpReq: FromServiceRequest : CachableKey : ClassTag, HttpRes: ToServiceResponse : CachableResult : ClassTag]
(implicit
 val async: Async[M],
 timeService: NanoTimeService,
 makeHttpService: MakeHttpService[M, HttpReq, HttpRes],
 toJsonForHomePage: ToJson[HomePage],
 toJsonForEnrichedMostPopular: ToJson[EnrichedMostPopular],
 fromJsonForMostPopular: FromJson[MostPopular],
 fromJsonForPromotion: FromJson[Promotion],
 fromJsonForProgramme: FromJson[Programme],
 fromJsonForProduction: FromJson[Production]
) extends HttpReqHttpResServiceLanguageExtension[M, HttpReq, HttpRes] with PromotionServiceNames {

  val cachingStrategy = new DurationStaleCacheStategy(10000000, 10000000000l)
  val maxSize = new MaxMapSizeStrategy(1000, 100)



  val mostPopularHttp = http(mostPopularServiceName)

  val promotionHttp = http(promotionServiceName)

  val programmeAndProductionsHttp = http(programmeAndProductionServiceName)

  val rawMostPopularService = mostPopularHttp >--< profile("Most Popular") >--< objectify[MostPopularQuery, MostPopular]("mostPopular", ResponseProcessor.parsed) >--< caching[MostPopularQuery, MostPopular]("Most Popular", cachingStrategy, maxSize)
  val rawPromotionService = promotionHttp >--< caching("Promotion", cachingStrategy, maxSize) >--< profile("Promotion") >--< objectify[PromotionQuery, Promotion]("Promotion", ResponseProcessor.parsed)
  val rawProductionService = programmeAndProductionsHttp >--< objectify[ProductionId, Production]("Production", ResponseProcessor.parsed)
  val rawProgrammeService = programmeAndProductionsHttp >--< objectify[ProgrammeId, Programme]("Programme", ResponseProcessor.parsed)


  val enrichedPromotion = (rawPromotionService, rawProductionService).enrich[EnrichedPromotion]
  val enrichedMostPopular = (rawMostPopularService, rawProgrammeService).enrich[EnrichedMostPopular]

  val homePage = (enrichedPromotion, enrichedMostPopular).merge[HomePageQuery, HomePage]

  //
  //  val homePage2 = (
  //    (
  //      promotionHttp >--< caching("Promotion", cachingStrategy, maxSize) >--< profile("Promotion") >--< objectify[PromotionQuery, Promotion]("Promotion", ResponseProcessor.parsed),
  //      programmeAndProductionsHttp >--< objectify[ProductionId, Production]("Production", ResponseProcessor.parsed)
  //    ).enrich[EnrichedPromotion],
  //    (
  //      mostPopularHttp >--< profile("Most Popular") >--< objectify[MostPopularQuery, MostPopular]("mostPopular", ResponseProcessor.parsed) >--< caching[MostPopularQuery, MostPopular]("Most Popular", cachingStrategy, maxSize),
  //      programmeAndProductionsHttp >--< objectify[ProgrammeId, Programme]("Programme", ResponseProcessor.parsed)
  //    ).enrich[EnrichedMostPopular]
  //  ).merge[HomePageQuery, HomePage]

  val enrichedMostPopularEndPoint = enrichedMostPopular >--< endpoint("/mostPopular")
  val homePageEndPoint = homePage >--< endpoint("/homepage")

}

