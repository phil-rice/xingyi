package org.validoc.sample.domain

import scala.language.higherKinds

trait DomainFixture {
  val programmeId1 =  ProgrammeId("1",false)
  val programmeId2 = ProgrammeId("2",false)

  val programme1 = Programme("this is programme1")
  val programme2 = Programme("this is programme2")

  val mostPopular = MostPopular(Seq(programmeId1, programmeId2))
  val enrichedMostPopular = EnrichedMostPopular(Seq(programme1, programme2))

  private val productionId2 = ProductionId("2",false)
  val production2 = Production("someProduction")
  val promotion = Promotion(List(productionId2))
  val enrichedPromotion = EnrichedPromotion(Seq(production2))

  val homePage = HomePage(enrichedMostPopular, enrichedPromotion)
}


