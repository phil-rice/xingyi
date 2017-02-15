package org.validoc.domain

import org.validoc.utils.aggregate.{Enricher, HasChildren}
import org.validoc.utils.caching.{CachableKey, CachableResultUsingSucesses, Id, UnitId}
import org.validoc.utils.http.{Get, ServiceRequest, ToServiceRequest, Uri}
import org.validoc.utils.parser.ParserFinder

trait PromotionQuery

object PromotionQuery extends PromotionQuery {
  implicit object ToRequestForPromotionQuery extends ToServiceRequest[PromotionQuery] {
    override def apply(req: PromotionQuery): ServiceRequest =
      ServiceRequest(Get, Uri("http://someUri"))
  }
  implicit object CachableKeyForPromotionQuery extends CachableKey[PromotionQuery] {
    override def id(req: PromotionQuery): Id = UnitId

    override def bypassCache(req: PromotionQuery): Boolean = false
  }
  implicit def fromHomePageQuery(h: HomePageQuery) = PromotionQuery
}

case class Promotion(name: String, productionIds: List[ProductionId])

object Promotion {

  implicit object HasChildrenForPromotion extends HasChildren[Promotion, ProductionId] {
    override def apply(p: Promotion): Seq[ProductionId] = p.productionIds
  }

  implicit object CachableResultForPromotion extends CachableResultUsingSucesses[Promotion]

  implicit val ParserFinderForPromotion = ParserFinder.always(_ => Promotion("someName", List()))

}

case class EnrichedPromotion(name: String, productions: Seq[Production])

object EnrichedPromotion {

  implicit object EnricherForPromotion extends Enricher[EnrichedPromotion, Promotion, Production] {
    override def apply(p: Promotion)(children: Seq[Production]): EnrichedPromotion =
      EnrichedPromotion(p.name, children)
  }

}

