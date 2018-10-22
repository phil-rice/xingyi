package one.xingyi.sample.domain
import one.xingyi.core.UtilsSpec
import one.xingyi.core.aggregate.FindReq
import one.xingyi.core.cache.{CachableKey, UnitId}
import one.xingyi.core.domain.BypassCache
import one.xingyi.core.http.{FromServiceRequest, Get, ServiceRequest, Uri}
import one.xingyi.core.json.{JsonWriter, ToJson}
import one.xingyi.core.json.JsonWriterLangauge._
import one.xingyi.core.monad.IdentityMonad

import scala.reflect.ClassTag
import scala.util.Success

abstract class HomePageSpec[J: JsonWriter : ClassTag] extends UtilsSpec with DomainFixture {

  behavior of "HomePage for " + implicitly[ClassTag[J]].runtimeClass.getName

  it should " have a toJson" in {
    implicitly[ToJson[HomePage]].apply(homePage).noWhiteSpace shouldBe """{"mostPopular":{"programmes":["thisisprogramme1","thisisprogramme2"]},"promotions":{"productions":["someProduction"]}}"""
  }

  behavior of "HomePageQuery for " + implicitly[ClassTag[J]].runtimeClass.getName

  it should "have a fromServiceRequest that sets bypass cache to false" in {
    implicitly[FromServiceRequest[IdentityMonad, HomePageQuery]].apply(ServiceRequest(Get, Uri("/any"))).value shouldBe Success(HomePageQuery(false))
  }

  it should "have a 'FindReq' that turns a HomePageQuery into a PromotionQuery" in {
    val findReq = implicitly[FindReq[HomePageQuery, PromotionQuery]]
    findReq(HomePageQuery(false)) shouldBe PromotionQuery(false)
    findReq(HomePageQuery(true)) shouldBe PromotionQuery(true)
  }
  it should "have a 'FindReq' that turns a HomePageQuery into a MostPopularQuery" in {
    val findReq = implicitly[FindReq[HomePageQuery, MostPopularQuery]]
    findReq(HomePageQuery(false)) shouldBe MostPopularQuery(false)
    findReq(HomePageQuery(true)) shouldBe MostPopularQuery(true)
  }

  it should "have a CachableKey" in {
    val key = implicitly[CachableKey[HomePageQuery]]
    key.id(HomePageQuery(false)) shouldBe UnitId
    key.id(HomePageQuery(true)) shouldBe UnitId
    key.bypassCache(HomePageQuery(false)) shouldBe false
    key.bypassCache(HomePageQuery(true)) shouldBe true
  }

}