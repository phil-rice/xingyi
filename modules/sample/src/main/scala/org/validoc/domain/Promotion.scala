package org.validoc.domain

import org.validoc.utils.aggregate.{Enricher, HasChildren}

case class Promotion(name: String, productions: List[ProductionId])

object Promotion {

  implicit object HasChildrenForPromotion extends HasChildren[Promotion, ProductionId] {
    override def apply(p: Promotion): Seq[ProductionId] = p.productions
  }

}

case class EnrichedPromotion(name: String, productions: Seq[Production])

object EnrichedPromotion {
  implicit object EnricherForPromotion extends Enricher[EnrichedPromotion, Promotion, Production] {
    override def apply(p: Promotion)(children: Seq[Production]): EnrichedPromotion =
      EnrichedPromotion(p.name, children)
  }
}

