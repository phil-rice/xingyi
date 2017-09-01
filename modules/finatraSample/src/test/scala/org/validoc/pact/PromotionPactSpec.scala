package org.validoc.pact

import org.validoc.sample.domain._
import com.itv.scalapact.ScalaPactForger._
import com.twitter.util.Await

class PromotionPactSpec extends PactSpec {

  behavior of "HomePage Service"

  it should "return programmes when a programme id is sent" in {
    forgePact
      .between("Promotion")
      .and("ProductionAndProgramme")
      .addInteraction(programInteraction(1)).runConsumerTest { mockConfig =>
      httpServiceHolder.setPort(mockConfig.port)
      val service = promotionSetup.rawProgrammeService.service
      val id = ProgrammeId("1")
      Await.result(service(id)) shouldBe Programme("someProgramme1Info")
    }
  }


  it should "return productions when a production id is sent" in {
    forgePact
      .between("Promotion")
      .and("ProductionAndProgramme")
      .addInteraction(productionInteraction(1))
      .runConsumerTest { mockConfig =>
        httpServiceHolder.setPort(mockConfig.port)
        val service = promotionSetup.rawProductionService.service
        val id = ProductionId("1")
        Await.result(service(id)) shouldBe Production( """someProduction1Info""")
      }
  }
  it should "return the most popular list" in {
    forgePact
      .between("Promotion")
      .and("MostPopular")
      .addInteraction(mostPopular(1, 2, 3))
      .runConsumerTest {
        mockConfig =>
          httpServiceHolder.setPort(mockConfig.port)
          val service = promotionSetup.rawMostPopularService.service
          Await.result(service(MostPopularQuery)) shouldBe MostPopular(List(ProgrammeId("1"), ProgrammeId("2"), ProgrammeId("3")))
      }
  }

  it should "enrich the most popular with programme data - this is a bodge and needs sorting out, as everything is going to the same server..." in {
    forgePact
      .between("Promotion")
      .and("MostPopular")
      .addInteraction(mostPopular(1, 2, 3)).addInteraction(programInteraction(1)).addInteraction(programInteraction(2)).addInteraction(programInteraction(3))
      .runConsumerTest {
        mockConfig =>
          httpServiceHolder.setPort(mockConfig.port)
          val service = promotionSetup.enrichedMostPopular.service
          Await.result(service(MostPopularQuery)) shouldBe EnrichedMostPopular(List(Programme("someProgramme1Info"), Programme("someProgramme2Info"), Programme("someProgramme3Info")))
      }
  }

  it should "return promotions" in {
    forgePact
      .between("Promotion")
      .and("Promotion")
      .addInteraction(promotion(1, 2, 3))
      .runConsumerTest {
        mockConfig =>
          httpServiceHolder.setPort(mockConfig.port)
          val service = promotionSetup.rawPromotionService.service
          Await.result(service(PromotionQuery)) shouldBe Promotion(List(ProductionId("1"), ProductionId("2"), ProductionId("3")))
      }
  }

  it should "return enriched promotions" in {
    forgePact
      .between("Promotion")
      .and("Promotion")
      .addInteraction(promotion(1, 2, 3)).addInteraction(productionInteraction(1)).addInteraction(productionInteraction(2)).addInteraction(productionInteraction(3))
      .runConsumerTest {
        mockConfig =>
          httpServiceHolder.setPort(mockConfig.port)
          val service = promotionSetup.enrichedPromotion.service
          Await.result(service(PromotionQuery)) shouldBe EnrichedPromotion(List(Production("someProduction1Info"), Production("someProduction2Info"), Production("someProduction3Info")))
      }
  }

  it should "handle a home page query" in {
    forgePact
      .between("Promotion")
      .and("Promotion")
      .addInteraction(mostPopular(1, 2, 3)).addInteraction(programInteraction(1)).addInteraction(programInteraction(2)).addInteraction(programInteraction(3))
      .addInteraction(promotion(1, 2, 3)).addInteraction(productionInteraction(1)).addInteraction(productionInteraction(2)).addInteraction(productionInteraction(3))
      .runConsumerTest {
        mockConfig =>
          httpServiceHolder.setPort(mockConfig.port)
          val service = promotionSetup.homePage.service
          Await.result(service(HomePageQuery)) shouldBe
            HomePage(
              EnrichedMostPopular(List(Programme("someProgramme1Info"), Programme("someProgramme2Info"), Programme("someProgramme3Info"))),
              EnrichedPromotion(List(Production("someProduction1Info"), Production("someProduction2Info"), Production("someProduction3Info"))))
      }

  }

  def programInteraction(i: Int) = {
    interaction
      .description("Request for programme")
      .given(s"programme id [$i] exists")
      .uponReceiving(s"/programme/$i")
      .willRespondWith(200, s"""someProgramme${i}Info""")
  }

  def productionInteraction(i: Int) = {
    interaction
      .description("Request for production")
      .given(s"production id [$i] exists")
      .uponReceiving(s"/production/$i")
      .willRespondWith(200, s"""someProduction${i}Info""")
  }

  def mostPopular(i: Int*) = {
    val s = i.mkString("[", ",", "]")
    interaction
      .description("Request for most popular")
      .given(s"most popular is $s")
      .uponReceiving("/mostpopular")
      //.uponReceiving("/token/id/1")
      .willRespondWith(200, s)
  }

  def promotion(i: Int*) = {
    val s = i.mkString("[", ",", "]")
    interaction
      .description("Request for promotions")
      .given(s"promotions is $s")
      .uponReceiving("/promotion")
      //.uponReceiving("/token/id/1")
      .willRespondWith(200, s)
  }
}
