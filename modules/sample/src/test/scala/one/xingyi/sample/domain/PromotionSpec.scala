package one.xingyi.sample.domain
import one.xingyi.core.UtilsSpec
import one.xingyi.core.aggregate.{Enricher, HasChildren}
import one.xingyi.core.cache.{CachableKey, UnitId}
import one.xingyi.core.http._
import one.xingyi.core.json.{FromJson, JsonParser, JsonWriter, ToJson}
import one.xingyi.core.monad.IdentityMonad

import scala.reflect.ClassTag
import scala.util.Success

class PromotionSpec[J: JsonWriter : JsonParser : ClassTag] extends UtilsSpec with DomainFixture {

  behavior of "PromotionQuery for " + implicitly[ClassTag[J]].runtimeClass.getSimpleName

  it should "have a CachableKey" in {
    val key = implicitly[CachableKey[PromotionQuery]]
    key.id(PromotionQuery(false)) shouldBe UnitId
    key.id(PromotionQuery(true)) shouldBe UnitId
    key.bypassCache(PromotionQuery(false)) shouldBe false
    key.bypassCache(PromotionQuery(true)) shouldBe true
  }

  it should "have a ToServiceRequest" in {
    val toServiceRequest = implicitly[ToServiceRequest[PromotionQuery]]
    toServiceRequest(PromotionQuery(false)) shouldBe ServiceRequest(Get, Uri("/promotions"))
  }

  it should "have a FromServiceRequest" in {
    val fromServiceRequest = implicitly[FromServiceRequest[IdentityMonad, PromotionQuery]]
    fromServiceRequest(ServiceRequest(Get, Uri("/doesntmatter"))).value shouldBe Success(PromotionQuery(false))
  }

  behavior of "Promotion for " + implicitly[ClassTag[J]].runtimeClass.getSimpleName

  it should "have a HasChildren which returns the list of productionids" in {
    implicitly[HasChildren[Promotion, ProductionId]].apply(promotion) shouldBe List(productionId2)
  }

  it should "have a ToJson" in {
    implicitly[ToJson[Promotion]].apply(promotion).noWhiteSpace shouldBe """["2"]"""
  }

  it should "have a FromJson" in {
    implicitly[FromJson[Promotion]].apply("""{"details":["2"]}""") shouldBe promotion
  }

  behavior of "EnrichedPromotion for " + implicitly[ClassTag[J]].runtimeClass.getSimpleName

  it should "have  a toJson" in {
    implicitly[ToJson[EnrichedMostPopular]].apply(enrichedMostPopular).noWhiteSpace shouldBe """{"programmes":["thisisprogramme1","thisisprogramme2"]}"""
  }

  it should "Have an enricher" in {
    val enricher = implicitly[ Enricher[PromotionQuery, Promotion, ProductionId, Production, EnrichedPromotion]]
    enricher(PromotionQuery(false), promotion, List(productionId2->production2)) shouldBe enrichedPromotion
  }

}
