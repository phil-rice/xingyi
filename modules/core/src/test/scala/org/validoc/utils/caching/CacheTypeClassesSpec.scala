package org.validoc.utils.caching

import org.mockito.{Matchers, Mockito}
import org.scalatest.matchers.Matcher
import org.validoc.utils.UtilsSpec
import org.validoc.utils.cache._
import org.validoc.utils.map.{NoMapSizeStrategy, NoReportMapSizeReduction, ReportMapSizeReduction}

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

  behavior of "NoMapSizeStrategy"


  it should "do nothing" in {
    val strategy = mock[ReportMapSizeReduction]
    var cache = mock[Map[String, String]]

    NoMapSizeStrategy.modifyCache(cache, strategy)
    Mockito.verify(strategy, Mockito.times(0)).mapSizeChanges(org.mockito.Matchers.any[Int], org.mockito.Matchers.any[Iterable[String]])
  }

  behavior of "NoReportMapSizeReduction"

  it should "do nothing" in {
    NoReportMapSizeReduction.mapSizeChanges(1, Seq())
    NoReportMapSizeReduction.mapSizeChanges(1, Seq(1, 2, 3))
  }

}
