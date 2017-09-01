package org.validoc.pact

import com.twitter.finagle.Http.Client
import com.twitter.finagle.{Http, Service}
import com.twitter.finagle.http.{Request, Response}
import com.twitter.finatra.http
import com.twitter.finatra.utils.FuturePools
import com.twitter.util.Future
import org.validoc.finatra.FinatraAdapter
import org.validoc.sample.PromotionSetup
import org.validoc.sample.domain.SampleJsonsForCompilation
import org.validoc.utils.UtilsSpec
import org.validoc.utils.concurrency.Async
import org.validoc.utils.http.{MakeHttpService, ProtocolHostAndPort}

class HttpServiceHolder extends (Request => Future[Response]) {
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


abstract class PactSpec extends UtilsSpec with SampleJsonsForCompilation {
  val finatraAdapter = new FinatraAdapter(FuturePools.fixedPool(getClass.getName, 20))

  import finatraAdapter._

  val httpServiceHolder = new HttpServiceHolder
  //So this is a horrible bodge because the port sent here is ignored...
  implicit val makeHttpService = new MakeHttpService[Future, Request, Response] {
    override def apply(v1: ProtocolHostAndPort) = httpServiceHolder
  }

  val promotionSetup = new PromotionSetup[Future, Request, Response]
  //
  //  private val finatraServer = new FinatraServer(9000, new EndpointController(List(promotionSetup.homePageEndPoint, promotionSetup.enrichedMostPopularEndPoint)))
  ////  protected val server = new EmbeddedHttpServer(finatraServer)
}

