package org.validoc.sample.domain

import org.validoc.utils.json.{FromJson, ToJson}

trait SampleJsonsForCompilation {

  implicit val fromJsonForHomePageQuery = new FromJson[HomePageQuery] {
    override def apply(v1: String): HomePageQuery = HomePageQuery(false)
  }
  implicit val toJsonForHomePage = new ToJson[HomePage] {
    override def apply(v1: HomePage): String = v1.toString
  }
  implicit val toJsonForEnrichedMostPopular = new ToJson[EnrichedMostPopular] {
    override def apply(v1: EnrichedMostPopular): String = v1.toString
  }

  def listOfInts(v1: String) = v1.drop(1).dropRight(1).split(",").toList

  implicit val fromJsonForMostPopular = new FromJson[MostPopular] {
    override def apply(v1: String): MostPopular = MostPopular(listOfInts(v1).map(ProgrammeId(_, false)))
  }
  implicit val fromJsonForPromotion = new FromJson[Promotion] {
    override def apply(v1: String): Promotion = Promotion(listOfInts(v1).map(ProductionId(_, false)))
  }
  implicit val fromJsonForProgramme = new FromJson[Programme] {
    override def apply(v1: String): Programme = Programme(v1)
  }
  implicit val fromJsonForProduction = new FromJson[Production] {
    override def apply(v1: String): Production = Production(v1)
  }
  implicit val toJsonForProduction = new ToJson[Production] {
    override def apply(v1: Production): String = v1.toString
  }

}
