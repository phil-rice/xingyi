/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.sample.domain

import one.xingyi.core.aggregate.{Enricher, HasChildren}
import one.xingyi.core.cache.{CachableKey, ObjectId, UnitId}
import one.xingyi.core.domain.{BypassCache, DomainRequestCompanionQuery, DomainResponseCompanionObject}
import one.xingyi.core.http._
import one.xingyi.core.json._
import one.xingyi.core.language.Language._
import one.xingyi.core.monad.Monad

import scala.language.{higherKinds, implicitConversions}

case class PromotionQuery(bypassCache: Boolean) extends BypassCache

object PromotionQuery extends DomainRequestCompanionQuery[PromotionQuery] {

  implicit object CachableKeyForPromotionQuery extends CachableKey[PromotionQuery] {
    override def id(req: PromotionQuery) = UnitId
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

  implicit object ToJsonFoPromotion extends ToJsonLib[Promotion] with JsonWriterLangauge {
    override def apply(v1: Promotion) = toListT(v1.productionIds)
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
