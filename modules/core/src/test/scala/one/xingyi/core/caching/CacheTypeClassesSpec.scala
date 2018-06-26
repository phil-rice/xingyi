/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.caching

import org.mockito.{Matchers, Mockito}
import org.scalatest.matchers.Matcher
import one.xingyi.core.UtilsSpec
import one.xingyi.core.cache._
import one.xingyi.core.map.{NoMapSizeStrategy, NoReportMapSizeReduction, ReportMapSizeReduction}

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
