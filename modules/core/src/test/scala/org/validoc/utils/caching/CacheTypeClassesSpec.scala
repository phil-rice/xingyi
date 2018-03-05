package org.validoc.utils.caching

import org.validoc.utils.UtilsSpec
import org.validoc.utils.cache._

class CacheTypeClassesSpec extends UtilsSpec {

  behavior of "Default should cache"

  it should "cache everything" in {
    implicitly[ShouldUseCache[String]].apply("") shouldBe true
    implicitly[ShouldUseCache[Int]].apply(123) shouldBe true
  }

  behavior of "default cachable key"

  it should "not bypass cache and return objectid" in {
    implicitly[CachableKey[String]].bypassCache("123") shouldBe false
    implicitly[CachableKey[String]].id("123") shouldBe ObjectId("123")
  }
  behavior of "default cachable key for unit"

  it should "not bypass cache and return UnitId" in {
    implicitly[CachableKey[Unit]].bypassCache(()) shouldBe false
    implicitly[CachableKey[Unit]].id(()) shouldBe UnitId
  }
}
