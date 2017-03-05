package org.validoc.sample.domain

import org.validoc.utils.domain.{RoundTripTests, ServiceRequestResponseRoundTripTests, ToServiceResponseAndParserFinderRoundTripTests}
import org.validoc.utils.http.ContentType

trait DomainFixture {
  val programme1 = Programme(ProgrammeId("1"), "this is programme1")
  val programme2 = Programme(ProgrammeId("2"), "this is programme2")
  val enrichedMostPopular = EnrichedMostPopular(Seq(programme1, programme2))

  val production2 = Production(ProductionId("2"), "someProduction")
  val enrichedPromotion = EnrichedPromotion("somePromotion", Seq(production2))

  val homePage = HomePage(enrichedMostPopular, enrichedPromotion)
}


class HomePageServiceRequestParserFinderRoundTripTrips extends RoundTripTests[HomePage] with DomainFixture {
  override def makeSample = homePage

  override def contentTypes: Seq[ContentType] = Seq(ContentType("don't care"))
}

class EnrichedMostPopularServiceRequestParserFinderRoundTripTrips extends RoundTripTests[EnrichedMostPopular] with DomainFixture {
  override def makeSample = enrichedMostPopular

  override def contentTypes: Seq[ContentType] = Seq(ContentType("don't care"))
}

class EnrichedPromotionServiceRequestParserFinderRoundTripTrips extends RoundTripTests[EnrichedPromotion] with DomainFixture {
  override def makeSample = enrichedPromotion

  override def contentTypes: Seq[ContentType] = Seq(ContentType("don't care"))
}

