/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.sample.domain
import one.xingyi.core.UtilsSpec
import one.xingyi.core.aggregate.FindReq
import one.xingyi.core.cache.{CachableKey, UnitId}
import one.xingyi.core.http.{FromServiceRequest, Get, ServiceRequest, Uri}
import one.xingyi.core.json.{JsonWriter, ToJson}
import one.xingyi.core.monad.IdentityMonad

import scala.reflect.ClassTag
import scala.util.Success

abstract class HomePageSpec[J: JsonWriter : ClassTag] extends UtilsSpec with DomainFixture {

  behavior of "HomePage for " + implicitly[ClassTag[J]].runtimeClass.getName

  it should " have a toJson" in {
    implicitly
      [ToJson[HomePage]].apply(homePage).noWhiteSpace shouldBe """{"mostPopular":{"programmes":["thisisprogramme1","thisisprogramme2"]},"promotions":{"productions":[{"productionInfo":"someProduction"}]}}"""

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
