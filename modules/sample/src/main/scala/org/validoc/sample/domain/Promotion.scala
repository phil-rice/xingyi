package org.validoc.sample.domain

import org.validoc.utils.cache.Cachable
import org.validoc.utils.http.{Get, ServiceRequest, ToServiceRequest, Uri}
import org.validoc.utils.metrics.{MetricValue, ReportData}
import org.validoc.utils.tagless.{Enricher, HasChildren}

import scala.language.implicitConversions
import scala.util.Try

case class PromotionQuery(bypassCache: Boolean) extends BypassCache

object PromotionQuery extends DomainCompanionQuery[PromotionQuery] {

  implicit object CachableKeyForPromotionQuery extends Cachable[PromotionQuery] {
    override def apply(v1: PromotionQuery) = ()
  }

  implicit object ToServiceRequestForPromotionQuery extends ToServiceRequest[PromotionQuery] {
    override def apply(v1: PromotionQuery) = ServiceRequest(Get, Uri(s"/promotions"))
  }
}

case class Promotion(productionIds: List[ProductionId])

object Promotion extends DomainCompanionObject[PromotionQuery, Promotion] {

  implicit object HasChildrenForPromotion extends HasChildren[Promotion, ProductionId] {
    override def apply(p: Promotion): Seq[ProductionId] = p.productionIds
  }
}

case class EnrichedPromotion(productions: Seq[Production])

object EnrichedPromotion extends DomainCompanionObject[PromotionQuery, EnrichedPromotion] {
  implicit object EnricherForEP extends Enricher[PromotionQuery, Promotion, ProductionId, Production, EnrichedPromotion] {
    override def apply(v1: PromotionQuery, v2: Promotion, v3: Seq[(ProductionId, Production)]) = EnrichedPromotion(v3.map(_._2))
  }
}
