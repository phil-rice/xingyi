package org.validoc.sample.domain

import org.validoc.playJson.PlayJsonDomainObject
import org.validoc.utils.aggregate.{Enricher, HasChildren}
import org.validoc.utils.caching.{CachableKey, Id, UnitId}
import org.validoc.utils.http.{Get, ServiceRequest, Uri}
import play.api.libs.json.{Json, OFormat}
import scala.language.implicitConversions

trait PromotionQuery

object PromotionQuery extends PromotionQuery {
  implicit def toRequestForPromotionQuery(req: PromotionQuery) = ServiceRequest(Get, Uri("http://someUri"))

  implicit object CachableKeyForPromotionQuery extends CachableKey[PromotionQuery] {
    override def id(req: PromotionQuery): Id = UnitId

    override def bypassCache(req: PromotionQuery): Boolean = false
  }

  implicit def fromHomePageQuery(h: HomePageQuery) = PromotionQuery
}

case class Promotion(name: String, productionIds: List[ProductionId])

object Promotion extends PlayJsonDomainObject[Promotion] {
  implicit val modelFormat: OFormat[Promotion] = Json.format[Promotion]

  implicit object HasChildrenForPromotion extends HasChildren[Promotion, ProductionId] {
    override def apply(p: Promotion): Seq[ProductionId] = p.productionIds
  }


}

case class EnrichedPromotion(name: String, productions: Seq[Production])

object EnrichedPromotion extends PlayJsonDomainObject[EnrichedPromotion] {
  implicit val modelFormat: OFormat[EnrichedPromotion] = Json.format[EnrichedPromotion]

  implicit object EnricherForPromotion extends Enricher[EnrichedPromotion, Promotion, Production] {
    override def apply(p: Promotion)(children: Seq[Production]): EnrichedPromotion =
      EnrichedPromotion(p.name, children)
  }

}

