package org.validoc

import org.validoc.domain._
import org.validoc.utils.concurrency.Async
import org.validoc.utils.http._
import org.validoc.utils.service.{IHttpSetup, MakeHttpService, ServiceInterpreters, StringServiceTag}
import org.validoc.utils.time.{NanoTimeService, SystemClockNanoTimeService}

import scala.concurrent.duration._


class PromotionSetup[Tag[M[_], _, _], M[_], HttpReq, HttpRes: ToServiceResponse](implicit toHttpReq: (ServiceRequest) => HttpReq, nanoTimeService: NanoTimeService, makeHttpService: MakeHttpService[M, HttpReq, HttpRes], s: IHttpSetup[Tag, M, HttpReq, HttpRes]) {

  type Setup = IHttpSetup[Tag, M, HttpReq, HttpRes]

  val mostPopularHttp = s.rawService("mostPopular")

  val promotionHttp = s.rawService("promotion")

  val programmeAndProductionsHttp = s.rawService("programmeAndProductions")

  val enrichedMostPopularService = {
    import s._
    (aggregate(
      getCachedProfiledObject[MostPopularQuery, MostPopular](2 minutes, 10 hours, 20, mostPopularHttp),
      getCachedProfiledObject[ProgrammeId, Programme](2 minutes, 10 hours, 2000, programmeAndProductionsHttp)).
      enrich[EnrichedMostPopular])
  }

  val enrichedPromotionService = {
    import s._
    aggregate(
      cached[PromotionQuery, Promotion](2 minutes, 10 hours, 20)(profiled(httpCallout(promotionHttp))),
      //      getCachedProfiledObject[HomePageQuery, Promotion](2 minutes, 10 hours, 20, promotionHttp),
      getCachedProfiledObject[ProductionId, Production](2 minutes, 10 hours, 2000, programmeAndProductionsHttp)).
      enrich[EnrichedPromotion]
  }

  val homePageService = {
    import s._
    endpoint0("/endpoint")(
      aggregate(
        enrichedMostPopularService,
        enrichedPromotionService).
        merge[HomePageQuery, HomePage](HomePage.apply))
  }
}


object Sample3 extends App {

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
    override def create(name: String): (String) => Option[String] = (req => Some(s"HttpService($req)"))
  }

  val setup = new PromotionSetup[StringServiceTag, Option, String, String]()

  import setup._

  println(enrichedMostPopularService)
  println
  println(homePageService)
  println
  println(enrichedPromotionService)
}
