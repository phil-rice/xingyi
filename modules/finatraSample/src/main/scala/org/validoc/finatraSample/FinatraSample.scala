package org.validoc.finatraSample

import com.twitter.finagle.http.{Request, Response}
import com.twitter.finatra.utils.FuturePools
import com.twitter.util.Future
import org.validoc.finatra._
import org.validoc.language.{AggregatedServicesInterpreter, ServiceData}
import org.validoc.sample.PromotionSetup
import org.validoc.sample.domain._
import org.validoc.utils.http.MakeHttpService
import org.validoc.utils.json.{FromJson, ToJson}

object FinatraSample extends FinatraAdapter()(FuturePools.fixedPool("pool", 20)) with App with SampleJsonsForCompilation {

  implicit val makeHttpService = MakeHttpService(MockFinatraService("mostPopular", "promotion", "programmeAndProductions"))

  val setup = new PromotionSetup[ServiceData, Future, Request, Response](new AggregatedServicesInterpreter)

  val sd = setup.homePageService
  println(sd)
  new FinatraServer(8080, new PingController, new EndpointController(sd)).main(args)

}
