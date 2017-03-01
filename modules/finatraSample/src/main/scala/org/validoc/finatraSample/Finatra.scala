package org.validoc.finatraSample

import com.twitter.finagle.http.{Request, Response}
import com.twitter.util.{Future, FuturePool}
import org.validoc.PromotionSetup
import org.validoc.finatra.{EndpointController, FinatraServer, MockFinatraService, PingController}
import org.validoc.language.{MakeHttpService, ServiceData, ServiceInterpreters}
import org.validoc.utils.metrics.NullPutMetrics
import org.validoc.utils.success.{Succeeded, SucceededFromFn}
import org.validoc.utils.time.SystemClockNanoTimeService


object Finatra extends App {

  import org.validoc.finatra.FinatraPlayground._

  implicit val nanoTimeService = SystemClockNanoTimeService
  implicit val makeHttpService = MakeHttpService(MockFinatraService("mostPopular", "promotion", "programmeAndProductions"))
  implicit val futurePool = FuturePool.unboundedPool
  implicit val serviceData = new ServiceInterpreters.ServicesGroupedForAsync[Future, Request, Response]
  implicit val putMetrics = NullPutMetrics
  implicit val succeeded = new SucceededFromFn[Response](_.getStatusCode() / 100 == 2
  )
  val setup = new PromotionSetup[ServiceData, Future, Request, Response]

  import setup._

  val sd = homePageService
  println(sd)
  new FinatraServer(8080, new PingController, new EndpointController(sd)).main(args)

}
