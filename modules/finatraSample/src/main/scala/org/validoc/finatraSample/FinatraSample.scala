package org.validoc.finatraSample

import com.twitter.finagle.http.{Request, Response}
import com.twitter.finatra.utils.FuturePools
import com.twitter.util.Future
import org.validoc.finatra._
import org.validoc.language.{AggregatedServicesInterpreter, ServiceData}
import org.validoc.sample.{PromotionSetup, Sample4}
import org.validoc.sample.domain._
import org.validoc.utils.http.MakeHttpService
import org.validoc.utils.json.{FromJson, ToJson}
import org.validoc.utils.service.ServicesSummary

object FinatraSample extends FinatraAdapter()(FuturePools.fixedPool("pool", 20)) with App with SampleJsonsForCompilation {

  implicit val makeHttpService = MakeHttpService(MockFinatraService("mostPopular", "promotion", "programmeAndProductions"))

  val sample = new Sample4[Future, Request, Response]
  val setup = new PromotionSetup[ServiceData, Future, Request, Response](new AggregatedServicesInterpreter)

  val sd = sample.homePageEndpoint // setup.homePageService
  println(sd)
  new FinatraServer(8080, new PingController, new EndpointController(ServicesSummary(sd))).main(args)

}
