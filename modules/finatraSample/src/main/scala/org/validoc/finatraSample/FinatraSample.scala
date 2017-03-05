package org.validoc.finatraSample

import com.twitter.finagle.http.{Request, Response}
import com.twitter.finatra.utils.FuturePools
import com.twitter.util.Future
import org.validoc.PromotionSetup
import org.validoc.finatra._
import org.validoc.language.{AggregatedServicesInterpreter, MakeHttpService, ServiceData}

object FinatraSample extends FinatraAdapter()(FuturePools.fixedPool("pool", 20)) with App  {

  implicit val makeHttpService = MakeHttpService(MockFinatraService("mostPopular", "promotion", "programmeAndProductions"))

  val setup = new PromotionSetup[ServiceData, Future, Request, Response](new AggregatedServicesInterpreter)

  val sd = setup.homePageService
  println(sd)
  new FinatraServer(8080, new PingController, new EndpointController(sd)).main(args)

}
