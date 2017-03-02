package org.validoc

import org.validoc.domain._
import org.validoc.language.{IHttpSetup, MakeHttpService, ServiceInterpreters, StringServiceTag}
import org.validoc.utils.http._
import org.validoc.utils.metrics.{MetricValue, PutMetrics}
import org.validoc.utils.success.{Succeeded, SucceededFromFn, SucceededState}
import org.validoc.utils.time.{NanoTimeService, SystemClockNanoTimeService}

import scala.language.postfixOps
import scala.concurrent.duration._
import scala.util.Try


class PromotionSetup[Tag[M[_], _, _], M[_], HttpReq, HttpRes: ToServiceResponse](implicit toHttpReq: (ServiceRequest) => HttpReq, nanoTimeService: NanoTimeService, makeHttpService: MakeHttpService[M, HttpReq, HttpRes], putMetrics: PutMetrics, succeeded: Succeeded[HttpRes], s: IHttpSetup[Tag, M, HttpReq, HttpRes]) {

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
    import s._
    import org.validoc.utils.functions.Functions._
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

trait SampleForStrings {

  implicit object ServiceResponseForString extends ToServiceResponse[String] {
    override def apply(v1: String): ServiceResponse = ServiceResponse(Status.Ok, Body(v1), ContentType("text/plain"))
  }

  implicit object StringToServiceRequest extends (String => ServiceRequest) {
    override def apply(v1: String): ServiceRequest = ServiceRequest(Get, Uri(v1))
  }

  implicit object ServiceRequestToString extends (ServiceRequest => String) {
    override def apply(v1: ServiceRequest): String = v1.uri.asUriString
  }

  implicit val printer = new ServiceInterpreters.ServiceToString[Option, String, String]

  implicit val nanoTimeService = SystemClockNanoTimeService

  implicit object MakeHttpServiceForString extends MakeHttpService[Option, String, String] {
    override def create(name: String): (String) => Option[String] = req => Some(s"HttpService($req)")
  }

  implicit object SucceededForString extends SucceededFromFn[String](_ => true)

  implicit object PutMetricsForString extends PutMetrics {
    override def apply(v1: Map[String, MetricValue]): Unit = {}
  }

}


object Sample3 extends App with SampleForStrings {

  val setup = new PromotionSetup[StringServiceTag, Option, String, String]()

  import setup._

  println(enrichedMostPopularService)
  println
  println(homePageService)
  println
  println(enrichedPromotionService)
}
