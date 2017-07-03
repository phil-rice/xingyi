package org.validoc.sample

import org.validoc.sample.domain._
import org.validoc.utils.aggregate.EnrichParentChildService
import org.validoc.utils.caching.{CachableKey, CachableResult}
import org.validoc.utils.concurrency.Async
import org.validoc.utils.http._
import org.validoc.utils.retry.RetryConfig
import org.validoc.utils.service.{HttpServiceCompositionLanguage, MakeServiceMakerForTwoServices, ServiceDescription}
import org.validoc.utils.time.RandomDelay
import org.validoc.utils.{FromServiceRequest, ToServiceResponse}

import scala.language.higherKinds
import scala.concurrent.duration._
import scala.language.postfixOps

class Sample4[M[_] : Async, HttpReq: FromServiceRequest : CachableKey, HttpRes: ToServiceResponse : CachableResult](implicit makeHttpService: MakeHttpService[M, HttpReq, HttpRes])
  extends HttpServiceCompositionLanguage[M, HttpReq, HttpRes] {


  val retryConfig = RetryConfig(1, new RandomDelay(1 second))
  val mostPopularService = http(HostName("mostPopular"), Port(80)) --> asObject[MostPopularQuery, MostPopular] --> cache --> profile --> metrics("mostPopular")

  val promotionService = http(HostName("promotion"), Port(80)) --> asObject[PromotionQuery, Promotion] --> retry(retryConfig) --> cache --> profile --> metrics("promotion")

  val programmeAndProductionsHttp = http(HostName("programmeAndProductions"), Port(80)) --> retry(retryConfig) --> cache --> profile

  val programmeService = programmeAndProductionsHttp --> asObject[ProgrammeId, Programme] --> metrics("programme")

  val productionService = programmeAndProductionsHttp --> asObject[ProductionId, Production] --> metrics("production")

  val enrichMostPopularService = (mostPopularService aggregate programmeService).enrich[EnrichedMostPopular]

  val enrichPromotionService = (promotionService aggregate productionService).enrich[EnrichedPromotion]

  val homePageService: ServiceDescription[M, HomePageQuery, HomePage] = (enrichPromotionService aggregate enrichMostPopularService).merge[HomePageQuery, HomePage]

  val homePageEndpoint = homePageService --> endpoint("/homepage")

  val enrichMostPopularEndpoint = homePageService --> endpoint("/mostPopular")
}

object Sample4 extends App {

}
