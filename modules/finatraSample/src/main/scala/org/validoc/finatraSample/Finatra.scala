package org.validoc.finatraSample

import com.twitter.finagle.http.{Request, Response}
import com.twitter.util.{Future, FuturePool}
import org.validoc.PromotionSetup
import org.validoc.domain.{EnrichedMostPopular, MostPopularQuery}
import org.validoc.finatra.{EndpointController, FinatraServer, MockFinatraService, PingController}
import org.validoc.utils.http._
import org.validoc.utils.service.ServiceInterpreters.ServicesGroupedForAsync
import org.validoc.utils.service.{ServiceData, ServiceInterpreters, StringServiceTag}
import org.validoc.utils.time.SystemClockNanoTimeService


object Finatra extends App {

  implicit object ServiceResponseForString extends ToServiceResponse[String] {
    override def apply(v1: String): ServiceResponse = ServiceResponse(Status.Ok, Body(v1), ContentType("text/plain"))
  }

  implicit object StringToServiceRequest extends (String => ServiceRequest) {
    override def apply(v1: String): ServiceRequest = ServiceRequest(Get, Uri(v1))
  }

  implicit object ServiceRequestToString extends (ServiceRequest => String) {
    override def apply(v1: ServiceRequest): String = v1.uri.asUriString
  }

  implicit val nanoTimeService = SystemClockNanoTimeService

  {
    implicit val printer = new ServiceInterpreters.ServiceToString[String, String]

    val setup = new PromotionSetup[String, String]()

    import setup._

    println(enrichedMostPopularService[StringServiceTag])
    println
    println(homePageService[StringServiceTag])
    println
  }

  implicit val futurePool = FuturePool.unboundedPool

  val services = MockFinatraService("mostPopular", "promotion", "programmeAndProductions")

  import org.validoc.finatra.FinatraPlayground._

  implicit val serviceData = new ServiceInterpreters.ServicesGroupedForAsync[Future, Request, Response](services).makeSetup

  val setup = new PromotionSetup[Request, Response]

  import setup._

  type AsyncServiceData[Req, Res] = ServiceData[Future, Req, Res, Request, Response]

  val sd = homePageService[AsyncServiceData]
  println(sd)
  new FinatraServer(8080, new PingController, new EndpointController(sd)).main(args)

}
