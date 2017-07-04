package org.validoc.sample

import org.validoc.sample.domain._
import org.validoc.utils.caching.{CachableKey, CachableResult}
import org.validoc.utils.concurrency.Async
import org.validoc.utils.http._
import org.validoc.utils.json.{FromJson, ToJson}
import org.validoc.utils.retry.RetryConfig
import org.validoc.utils.service.{HttpServiceCompositionLanguage, ServiceDescription}
import org.validoc.utils.time.RandomDelay

import scala.concurrent.duration._
import scala.language.{higherKinds, postfixOps}

class Sample4[M[_] : Async, HttpReq: FromServiceRequest : CachableKey, HttpRes: ToServiceResponse : CachableResult]
(implicit makeHttpService: MakeHttpService[M, HttpReq, HttpRes],
 fromJsonForHomePageQuery: FromJson[HomePageQuery],
 toJsonForHomePage: ToJson[HomePage],
 toJsonForEnrichedMostPopular: ToJson[EnrichedMostPopular],
 fromJsonForMostPopular: FromJson[MostPopular],
 fromJsonForPromotion: FromJson[Promotion],
 fromJsonForProgramme: FromJson[Programme],
 fromJsonForProduction: FromJson[Production]

)
  extends HttpServiceCompositionLanguage[M, HttpReq, HttpRes] {
  // Note that >-< means 'wrap with'

  val retryConfig = RetryConfig(1, new RandomDelay(1 second))
  val mostPopularService = http(HostName("mostPopular"), Port(80)) >-< asObject[MostPopularQuery, MostPopular] >-< cache >-< profile >-< metrics("mostPopular")


  val promotionService = http(HostName("promotion"), Port(80)) >-< asObject[PromotionQuery, Promotion] >-< retry(retryConfig) >-< cache >-< profile >-< metrics("promotion")


  val programmeAndProductionsHttp = http(HostName("programmeAndProductions"), Port(80)) >-< retry(retryConfig) >-< cache >-< profile


  val programmeService = programmeAndProductionsHttp >-< asObject[ProgrammeId, Programme] >-< metrics("programme")


  val productionService = programmeAndProductionsHttp >-< asObject[ProductionId, Production] >-< metrics("production")

  val enrichMostPopularService = (mostPopularService aggregate programmeService).enrich[EnrichedMostPopular]

  val enrichPromotionService = (promotionService aggregate productionService).enrich[EnrichedPromotion]

  val homePageService: ServiceDescription[M, HomePageQuery, HomePage] = (enrichPromotionService aggregate enrichMostPopularService).merge[HomePageQuery, HomePage]

  val homePageEndpoint = homePageService >-< endpoint[M, HomePageQuery, HomePage]("/homepage")

  val enrichMostPopularEndpoint = enrichMostPopularService >-< endpoint[M, MostPopularQuery, EnrichedMostPopular]("/mostPopular")
}

object Sample4 extends App {

}
