package one.xingyi.sample.domain

import one.xingyi.core.aggregate.{Enricher, HasChildren}
import one.xingyi.core.cache.{CachableKey, ObjectId}
import one.xingyi.core.domain.{BypassCache, DomainRequestCompanionQuery, DomainResponseCompanionObject}
import one.xingyi.core.functions.Monad
import one.xingyi.core.http._
import one.xingyi.core.json._
import one.xingyi.core.language.Language._

import scala.language.{higherKinds, implicitConversions}

case class PromotionQuery(bypassCache: Boolean) extends BypassCache

object PromotionQuery extends DomainRequestCompanionQuery[PromotionQuery] {

  implicit object CachableKeyForPromotionQuery extends CachableKey[PromotionQuery] {
    override def id(req: PromotionQuery) = ObjectId(req)
    override def bypassCache(req: PromotionQuery) = req.bypassCache
  }

  implicit object ToServiceRequestForPromotionQuery extends ToServiceRequest[PromotionQuery] {
    override def apply(v1: PromotionQuery) = ServiceRequest(Get, Uri(s"/promotions"))
  }

  implicit def fromServiceRequest[M[_] : Monad]: FromServiceRequest[M, PromotionQuery] = new FromServiceRequest[M, PromotionQuery] {
    override def apply(v1: ServiceRequest) = PromotionQuery(false).liftM
  }
}


case class Promotion(productionIds: List[ProductionId])

object Promotion extends DomainResponseCompanionObject[PromotionQuery, Promotion] {

  implicit object HasChildrenForPromotion extends HasChildren[Promotion, ProductionId] {
    override def apply(p: Promotion): Seq[ProductionId] = p.productionIds
  }

  implicit object ToJsonFoPromotion extends ToJson[Promotion] {
    override def apply(v1: Promotion) = s"""{id: [${v1.productionIds.map(id => s""""$id"""").mkString(",")}]"""
  }

  implicit def fromJsonFromPromotion[J: JsonParser](implicit forId: FromJsonLib[J, ProductionId]): FromJsonLib[J, Promotion] =
    json => Promotion((json \ "details").asList[ProductionId])
}

case class EnrichedPromotion(productions: Seq[Production])

object EnrichedPromotion extends DomainResponseCompanionObject[PromotionQuery, EnrichedPromotion] {
  implicit object EnricherForEP extends Enricher[PromotionQuery, Promotion, ProductionId, Production, EnrichedPromotion] {
    override def apply(v1: PromotionQuery, v2: Promotion, v3: Seq[(ProductionId, Production)]) = EnrichedPromotion(v3.map(_._2))
  }
  implicit def toJsonForEP[J: JsonWriter](implicit forProduction: ToJsonLib[Production]): ToJsonLib[EnrichedPromotion] = p => JsonObject("productions" -> (p.productions: JsonList))

}
