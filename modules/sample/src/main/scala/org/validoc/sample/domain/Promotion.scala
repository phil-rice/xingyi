package org.validoc.sample.domain

import org.validoc.utils.aggregate.{Enricher, HasChildren}
import org.validoc.utils.caching.{CachableKey, Id, UnitId}
import org.validoc.utils.http.{Get, ServiceRequest, ToServiceRequest, Uri}
import org.validoc.utils.metrics.{MetricValue, ReportData}

import scala.language.implicitConversions
import scala.util.Try

trait PromotionQuery

object PromotionQuery extends DomainCompanionObject[PromotionQuery] with PromotionQuery {

  implicit object ToServiceRquestForPromotionQuery extends ToServiceRequest[PromotionQuery] {
    override def apply(v1: PromotionQuery): ServiceRequest = ServiceRequest(Get, Uri("http://someUri"))
  }


  implicit object CachableKeyForPromotionQuery extends CachableKey[PromotionQuery] {
    override def id(req: PromotionQuery): Id = UnitId

    override def bypassCache(req: PromotionQuery): Boolean = false
  }

  implicit def fromHomePageQuery(h: HomePageQuery) = PromotionQuery
}

case class Promotion(name: String, productionIds: List[ProductionId])

object Promotion extends DomainCompanionObject[Promotion] {

  implicit object ReportDataForPromotion extends ReportData[Promotion] {
    override def apply(v1: String, v2: Try[Promotion], v3: Long): Map[String, MetricValue] = Map()
  }

  implicit object HasChildrenForPromotion extends HasChildren[Promotion, ProductionId] {
    override def apply(p: Promotion): Seq[ProductionId] = p.productionIds
  }


}

case class EnrichedPromotion(name: String, productions: Seq[Production])

object EnrichedPromotion extends DomainCompanionObject[EnrichedPromotion] {

  implicit object EnricherForPromotion extends Enricher[EnrichedPromotion, Promotion, Production] {
    override def apply(p: Promotion, children: Seq[Production]): EnrichedPromotion =
      EnrichedPromotion(p.name, children)
  }

}

