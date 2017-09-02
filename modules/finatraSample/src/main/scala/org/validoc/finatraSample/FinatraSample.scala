package org.validoc.finatraSample

import com.twitter.finagle.Http
import com.twitter.finagle.http.{Request, Response}
import com.twitter.finatra.utils.FuturePools
import com.twitter.util.{Future, FuturePool}
import org.validoc.finatra._
import org.validoc.sample.{PromotionServiceNames, PromotionSetup}
import org.validoc.sample.domain._
import org.validoc.utils.http.{MakeHttpService, ProtocolHostAndPort, ServiceName}

class FinatraPromotionSetup(futurePool: FuturePool)(implicit val makeHttpService: MakeHttpService[Future, Request, Response]) extends FinatraAdapter(futurePool) with SampleJsonsForCompilation {
  val fp1 = implicitly[FuturePool]


  val sample = new PromotionSetup[Future, Request, Response]

  val endPoints = List(sample.homePageEndPoint, sample.enrichedMostPopularEndPoint)
}

object FinatraSample extends App with PromotionServiceNames {
  val config = Map[ServiceName, ProtocolHostAndPort](
    programmeAndProductionServiceName -> ProtocolHostAndPort("localhost", 2000),
    promotionServiceName -> ProtocolHostAndPort("localhost", 2001),
    mostPopularServiceName -> ProtocolHostAndPort("localhost", 2002)
  )
  implicit val makeHttpService = new MakeHttpService[Future, Request, Response] {
    override def apply(v1: ServiceName) = {
      val ProtocolHostAndPort(protocol, host, port) = config(v1)
      Http.newService(s"$host:$port")
    }
  }

  val setup = new FinatraPromotionSetup(FuturePools.fixedPool("FuturePoolForApp", 20))

  //  val setup = new PromotionSetup[ServiceData, Future, Request, Response](new AggregatedServicesInterpreter)
  //  val sd = setup.homePageService


  new FinatraServer(8080, new PingController, new EndpointController(setup.endPoints)).main(args)

}
