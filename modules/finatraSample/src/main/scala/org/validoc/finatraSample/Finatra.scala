package org.validoc.finatraSample

import com.twitter.finagle.http.{Request, Response}
import com.twitter.util.{Future, FuturePool}
import org.validoc.PromotionSetup
import org.validoc.domain.{EnrichedMostPopular, MostPopularQuery}
import org.validoc.finatra.{EndpointController, FinatraServer, MockFinatraService, PingController}
import org.validoc.utils.http._
import org.validoc.utils.service.ServiceInterpreters.ServicesGroupedForAsync
import org.validoc.utils.service.{MakeHttpService, ServiceData, ServiceInterpreters, StringServiceTag}
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
    implicit val printer = new ServiceInterpreters.ServiceToString[Future, String, String]

    def identity[X] = (x: X) => Future(x)

    implicit val makeHttpService = MakeHttpService[Future, String, String](Map("mostPopular"->identity, "promotion"->identity, "programmeAndProductions"->identity))

    val setup = new PromotionSetup[Future, String, String]()

    import setup._
    println(enrichedMostPopularService[StringServiceTag])
    println
    println(homePageService[StringServiceTag])
    println
  }

  implicit val makeHttpService = MakeHttpService(MockFinatraService("mostPopular", "promotion", "programmeAndProductions"))

  implicit val futurePool = FuturePool.unboundedPool

  import org.validoc.finatra.FinatraPlayground._

  implicit val serviceData = new ServiceInterpreters.ServicesGroupedForAsync[Future, Request, Response].makeSetup

  val setup = new PromotionSetup[Future, Request, Response]

  import setup._
  type AsyncServiceData[Req, Res] = ServiceData[Future, Req, Res]
  val sd = homePageService[AsyncServiceData]
  println(sd)
  new FinatraServer(8080, new PingController, new EndpointController(sd)).main(args)

}
