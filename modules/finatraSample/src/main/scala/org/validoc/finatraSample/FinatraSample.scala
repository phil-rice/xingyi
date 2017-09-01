package org.validoc.finatraSample

import com.twitter.finagle.http.{Request, Response}
import com.twitter.finatra.utils.FuturePools
import com.twitter.util.{Future, FuturePool}
import org.validoc.finatra._
import org.validoc.sample.PromotionSetup
import org.validoc.sample.domain._
import org.validoc.utils.http.MakeHttpService

class FinatraPromotionSetup(futurePool: FuturePool) extends FinatraAdapter(futurePool) with SampleJsonsForCompilation {
  val fp1 = implicitly[FuturePool]
  implicit val makeHttpService = MakeHttpService(MockFinatraService("mostPopular", "promotion", "programmeAndProductions"))


  val sample = new PromotionSetup[Future, Request, Response]

  val endPoints = List(sample.homePageEndPoint, sample.enrichedMostPopularEndPoint)
}

object FinatraSample extends App {

  val setup = new FinatraPromotionSetup(FuturePools.fixedPool("FuturePoolForApp", 20))

  //  val setup = new PromotionSetup[ServiceData, Future, Request, Response](new AggregatedServicesInterpreter)
  //  val sd = setup.homePageService


  new FinatraServer(8080, new PingController, new EndpointController(setup.endPoints)).main(args)

}
