package org.validoc.pact

import com.itv.scalapact.ScalaPactForger.{ScalaPactInteraction, forgePact, interaction}
import com.twitter.finagle.Http.Client
import com.twitter.finagle.{Http, Service}
import com.twitter.finagle.http.{Request, Response}
import com.twitter.finatra.http
import com.twitter.finatra.utils.FuturePools
import com.twitter.util.Future
import org.scalatest.BeforeAndAfter
import org.validoc.finatra.FinatraAdapter
import org.validoc.finatraSample.FinatraPromotionSetup
import org.validoc.finatraSample.FinatraSample.{mostPopularServiceName, programmeAndProductionServiceName, promotionServiceName}
import org.validoc.sample.PromotionSetup
import org.validoc.sample.domain.SampleJsonsForCompilation
import org.validoc.utils.UtilsSpec
import org.validoc.utils.caching.CachingService
import org.validoc.utils.concurrency.Async
import org.validoc.utils.http.{MakeHttpService, ProtocolHostAndPort, ServiceName}

class HttpServiceHolder extends (Request => Future[Response]) {
  def clear = service = None

  var service: Option[Service[Request, Response]] = None

  def setPort(port: Int) = {
    service match {
      case Some(s) => s.close()
      case _ =>
    }
    service = Some(Http.client.newService(s"localhost:$port"))
  }

  override def apply(v1: Request) = service.get(v1)
}

abstract class PactSpec extends UtilsSpec with SampleJsonsForCompilation with BeforeAndAfter {

  val config: Map[ServiceName, HttpServiceHolder] = Map[ServiceName, HttpServiceHolder](
    programmeAndProductionServiceName -> new HttpServiceHolder,
    promotionServiceName -> new HttpServiceHolder,
    mostPopularServiceName -> new HttpServiceHolder
  )
  //So this is a horrible bodge because the port sent here is ignored...
  implicit val makeHttpService = MakeHttpService(config)


  val promotionSetup = new FinatraPromotionSetup(FuturePools.fixedPool(getClass.getName, 20))

  after {
    config.values.foreach(_.clear)
    promotionSetup.sample.homePage.findAllServices[CachingService[Future, _, _]].foreach(_.clearCache)
  }

  def makePact(provider: ServiceName, interaction: ScalaPactInteraction*)(block: => Unit) = {
    interaction.foldLeft(forgePact.between("HomePage").and(provider.name))((acc, i) => acc.addInteraction(i)).runConsumerTest {
      mockConfig =>
        config(provider).setPort(mockConfig.port)
        block
    }
  }

  def programInteraction(i: Int) = {
    interaction
      .description(s"Request for programme $i")
      .given(s"programme id [$i] exists")
      .uponReceiving(s"/programme/$i")
      .willRespondWith(200,
        s"""someProgramme${
          i
        }Info""")
  }

  def productionInteraction(i: Int) = {
    interaction
      .description(s"Request for production $i")
      .given(s"production id [$i] exists")
      .uponReceiving(s"/production/$i")
      .willRespondWith(200,
        s"""someProduction${
          i
        }Info""")
  }

  def mostPopular(i: Int*) = {
    val s = i.mkString("[", ",", "]")
    interaction
      .description(s"Request for most popular $s")
      .given(s"most popular is $s")
      .uponReceiving("/mostpopular")
      //.uponReceiving("/token/id/1")
      .willRespondWith(200, s)
  }

  def promotion(i: Int*) = {
    val s = i.mkString("[", ",", "]")
    interaction
      .description(s"Request for promotions $s")
      .given(s"promotions is $s")
      .uponReceiving("/promotion")
      //.uponReceiving("/token/id/1")
      .willRespondWith(200, s)
  }

}

