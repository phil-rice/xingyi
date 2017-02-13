package org.validoc.finatraSample

import com.sun.javafx.util.Logging
import com.twitter.finagle.http.Request
import com.twitter.finatra.http.Controller
import com.twitter.logging.Logging
import org.validoc.PromotionSetup
import org.validoc.finatra.FinatraServer
import org.validoc.utils.http._
import org.validoc.utils.service.ServiceInterpreters
import org.validoc.utils.time.SystemClockNanoTimeService

case class FinatraRequest()

class SampleController extends Controller {
  get("/") { request: FinatraRequest => "Hello World" }
  get("/req") { request: Request => "Hi" }
}

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

  implicit val printer = new ServiceInterpreters.ServiceToString[String, String]

  implicit val nanoTimeService = SystemClockNanoTimeService

  val setup = new PromotionSetup[String, String]()

  import setup._

  println(enrichedMostPopularService[String])
  println
  println(homePageService[String])
  println

  new FinatraServer(8080, new SampleController).main(args)

}
