package org.validoc.sample

import org.validoc.sample.domain._
import org.validoc.utils.caching.{CachableKey, CachableResult}
import org.validoc.utils.concurrency.{Async, MDCPropagatingExecutionContext}
import org.validoc.utils.http._
import org.validoc.utils.json.{FromJson, ToJson}
import org.validoc.utils.retry.RetryConfig
import org.validoc.utils.service.{AbstractServiceDescription, DebugBasePath, DebugEndPointResOps1, HttpServiceCompositionLanguage}
import org.validoc.utils.strings.IndentAndString
import org.validoc.utils.time.RandomDelay

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.language.{higherKinds, postfixOps}
import scala.util.Try

class Sample4[M[_] : Async, HttpReq: FromServiceRequest : CachableKey, HttpRes: ToServiceResponse : CachableResult]
(implicit makeHttpService: MakeHttpService[M, HttpReq, HttpRes],
 fromJsonForHomePageQuery: FromJson[HomePageQuery],
 toJsonForHomePage: ToJson[HomePage],
 toJsonForEnrichedMostPopular: ToJson[EnrichedMostPopular],
 fromJsonForMostPopular: FromJson[MostPopular],
 fromJsonForPromotion: FromJson[Promotion],
 fromJsonForProgramme: FromJson[Programme],
 fromJsonForProduction: FromJson[Production],
 toJsonForProduction: ToJson[Production]

)
  extends HttpServiceCompositionLanguage[M, HttpReq, HttpRes] {
  // Note that >-< means 'wrap with'
  implicit val d = DebugBasePath("/debug")

  val retryConfig = RetryConfig(1, new RandomDelay(1 second))
  val mostPopularService = http(HostName("mostPopular"), Port(80)) >-< asObject[MostPopularQuery, MostPopular] >-< cache >-< profile >-< metrics("mostPopular")


  val promotionService = http(HostName("promotion"), Port(80)) >-< asObject[PromotionQuery, Promotion] >-< retry(retryConfig) >-< cache >-< profile >-< metrics("promotion")


  val programmeAndProductionsHttp = http(HostName("programmeAndProductions"), Port(80)) >-< retry(retryConfig) >-< cache >-< profile


  val programmeService = programmeAndProductionsHttp >-< asObject[ProgrammeId, Programme] >-< metrics("programme")


  val productionService = programmeAndProductionsHttp >-< asObject[ProductionId, Production] >-< debug >-< metrics("production")

  val enrichMostPopularService = (mostPopularService aggregate programmeService).enrich[EnrichedMostPopular]

  val enrichPromotionService = (promotionService aggregate productionService).enrich[EnrichedPromotion]

  val homePageService: AbstractServiceDescription[M, HomePageQuery, HomePage] = (enrichPromotionService aggregate enrichMostPopularService).merge[HomePageQuery, HomePage]

  val homePageEndpoint = homePageService >-< endpoint[HomePageQuery, HomePage]("/homepage")

  val enrichMostPopularEndpoint = enrichMostPopularService >-< endpoint[MostPopularQuery, EnrichedMostPopular]("/mostPopular")
}

import org.validoc.utils.concurrency.Async._

object Sample4 extends App with SampleJsonsForCompilation {

  implicit val ec: MDCPropagatingExecutionContext = ExecutionContext.global
  implicit val httpServiceMaker = new MakeHttpService[Future, ServiceRequest, ServiceResponse] {
    override def create(hostName: HostName, port: Port): (ServiceRequest) => Future[ServiceResponse] = { sr => Future.successful(ServiceResponse(Status(200), Body(sr.toString), ContentType("someContentType"))) }
  }

  implicit object CachableResultForServiceResponse extends CachableResult[ServiceResponse] {
    override def shouldCacheStrategy(req: Try[ServiceResponse]): Boolean = req.isSuccess
  }

  object Setup extends Sample4[Future, ServiceRequest, ServiceResponse]

  println(Setup.homePageEndpoint.fold[IndentAndString]({ (sd, d) => IndentAndString(d, List((d, sd.shortToString))) }, 0))
  println
  println
  println
  Setup.homePageEndpoint.fold[List[AbstractServiceDescription[Future, _, _]]]({ (sd, d) => List(sd) }, 0).foreach(println)
}
