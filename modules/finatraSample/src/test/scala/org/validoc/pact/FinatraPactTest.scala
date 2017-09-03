package org.validoc.pact

import com.twitter.finagle.http.{Request, Response}
import com.twitter.finagle.{Http, Service}
import com.twitter.util.Future
import org.validoc.sample.PromotionSetup
import org.validoc.sample.pact.PromotionPactSpec
import org.validoc.finatra.FinatraAdapter._
import org.validoc.finatra.FinatraServer

class FinatraPactTest extends PromotionPactSpec[Future, Request, Response, Service[Request, Response], FinatraServer] {
  override def makeServiceOnLocalHost(port: Int) = Http.newService(s"localhost:$port")

  override def makePromotion() = new PromotionSetup()

  override val serviceTrees = List(promotionSetup.homePage, promotionSetup.enrichedMostPopular)

  println("FInatra Pact Test")
  println("FInatra Pact Test")
  println("FInatra Pact Test")
  println("FInatra Pact Test")
  println("FInatra Pact Test")
  println("FInatra Pact Test")
  println("FInatra Pact Test")
}
