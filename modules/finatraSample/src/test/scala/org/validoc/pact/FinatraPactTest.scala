package org.validoc.pact

import com.twitter.finagle.{Http, Service}
import com.twitter.finagle.http.{Request, Response}
import com.twitter.util.Future
import org.validoc.sample.PromotionSetup
import org.validoc.sample.pact.PromotionPactSpec
import org.validoc.utils.Closable
import org.validoc.utils.concurrency.Async._

object FinatraPactTest {

}

import FinatraPactTest._
import org.validoc.finatra.FinatraAdapter._

class FinatraPactTest extends PromotionPactSpec[Future, Request, Response, Service[Request, Response]] {
  override def makeServiceOnLocalHost(port: Int) = Http.newService(s"localhost:$port")

  override def makePromotion() = new PromotionSetup()

  override val serviceTrees = List(promotionSetup.homePage, promotionSetup.enrichedMostPopular)
}
