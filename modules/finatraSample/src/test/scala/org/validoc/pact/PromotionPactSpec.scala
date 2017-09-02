package org.validoc.pact

import com.itv.scalapact.ScalaPactForger._
import com.twitter.util.Await
import org.scalatest.BeforeAndAfterAll
import org.validoc.sample.PromotionServiceNames
import org.validoc.sample.domain._
import org.validoc.utils.http.ServiceName

class PromotionPactSpec extends PactSpec with PromotionServiceNames {

  import promotionSetup.sample._


  behavior of "HomePage Service"


  it should "return programmes when a programme id is sent" in {
    makePact(programmeAndProductionServiceName, programInteraction(1)) {
      Await.result(rawProgrammeService.service(ProgrammeId("1"))) shouldBe Programme("someProgramme1Info")
    }
  }

  it should "return productions when a production id is sent" in {
    makePact(programmeAndProductionServiceName, productionInteraction(1)) {
      Await.result(rawProductionService.service(ProductionId("1"))) shouldBe Production("""someProduction1Info""")
    }
  }

  it should "return the most popular list" in {
    makePact(mostPopularServiceName, mostPopular(2, 3)) {
      Await.result(rawMostPopularService.service(MostPopularQuery)) shouldBe MostPopular(List(ProgrammeId("2"), ProgrammeId("3")))
    }
  }

  it should "enrich the most popular with programme data " in {
    makePact(mostPopularServiceName, mostPopular(2, 3)) {
      makePact(programmeAndProductionServiceName, programInteraction(2), programInteraction(3)) {
        Await.result(enrichedMostPopular.service(MostPopularQuery)) shouldBe EnrichedMostPopular(List(Programme("someProgramme2Info"), Programme("someProgramme3Info")))
      }
    }
  }


  it should "return promotions" in {
    makePact(promotionServiceName, promotion(2, 3)) {
      Await.result(rawPromotionService.service(PromotionQuery)) shouldBe Promotion(List(ProductionId("2"), ProductionId("3")))
    }
  }

  it should "return enriched promotions" in {
    makePact(promotionServiceName, promotion(2, 3)) {
      makePact(programmeAndProductionServiceName, productionInteraction(2), productionInteraction(3)) {
        Await.result(enrichedPromotion.service(PromotionQuery)) shouldBe EnrichedPromotion(List(Production("someProduction2Info"), Production("someProduction3Info")))

      }
    }
  }

  it should "handle a home page query" in {
    makePact(promotionServiceName, promotion(4, 5)) {
      makePact(mostPopularServiceName, mostPopular(6, 7)) {
        makePact(programmeAndProductionServiceName, productionInteraction(4), productionInteraction(5), programInteraction(6), programInteraction(7)) {
          Await.result(homePage.service(HomePageQuery)) shouldBe
            HomePage(
              EnrichedMostPopular(List(Programme("someProgramme6Info"), Programme("someProgramme7Info"))),
              EnrichedPromotion(List(Production("someProduction4Info"), Production("someProduction5Info"))))
        }
      }
    }
  }

}
