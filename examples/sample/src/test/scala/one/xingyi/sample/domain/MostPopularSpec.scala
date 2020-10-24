/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.sample.domain
import one.xingyi.core.UtilsSpec
import one.xingyi.core.accessors.HasChildren
import one.xingyi.core.aggregate.Enricher
import one.xingyi.core.cache.{CachableKey, UnitId}
import one.xingyi.core.http._
import one.xingyi.core.json.{FromJson, JsonParser, JsonWriter, ToJson}
import one.xingyi.core.monad.IdentityMonad

import scala.reflect.ClassTag
import scala.util.Success


class MostPopularSpec[J: JsonParser : JsonWriter : ClassTag] extends UtilsSpec with DomainFixture {

  it should "have a CachableKey" in {
    val key = implicitly[CachableKey[MostPopularQuery]]
    key.id(MostPopularQuery(false)) shouldBe UnitId
    key.bypassCache(MostPopularQuery(false)) shouldBe false
    key.bypassCache(MostPopularQuery(true)) shouldBe true
  }

  it should "have a ToServiceRequest" in {
    val toRequest = implicitly[ToServiceRequest[MostPopularQuery]]
    toRequest(MostPopularQuery(false)) shouldBe ServiceRequest(Get, Uri("/mostpopular"))
  }

  it should "have a FromServiceRequest" in {
    val fromServiceRequest = implicitly[FromServiceRequest[IdentityMonad, MostPopularQuery]]
    fromServiceRequest(ServiceRequest(Get, Uri("/anything"))).value shouldBe Success(MostPopularQuery(false))
  }

  behavior of "MostPopular for " + implicitly[ClassTag[J]].runtimeClass.getName

  it should "have a most HasChildren that returns the contained programme ids" in {
    implicitly[HasChildren[MostPopular, ProgrammeId]] apply (mostPopular) shouldBe List(programmeId1, programmeId2)
  }

  it should "have a toJson" in {
    implicitly[ToJson[MostPopular]].apply(mostPopular).noWhiteSpace shouldBe """{id:["ProgrammeId(1,false)","ProgrammeId(2,false)"]"""
  }
  it should "have a FromJson" in {
    val json = s"""["${programmeId1.id}", "${programmeId2.id}"]"""
    implicitly[FromJson[MostPopular]].apply(json) shouldBe mostPopular
  }

  behavior of "EnrichedMostPopular for " + implicitly[ClassTag[J]].runtimeClass.getName

  it should "Be able to turn the querys and results into an enriched most popular" in {
    val enricher = implicitly[Enricher[MostPopularQuery, MostPopular, ProgrammeId, Programme, EnrichedMostPopular]]
    enricher(MostPopularQuery(false), mostPopular, List(programmeId1 -> programme1, programmeId2 -> programme2)) shouldBe enrichedMostPopular
  }

  it should "have a tojson" in {
    implicitly[ToJson[EnrichedMostPopular]].apply(enrichedMostPopular).noWhiteSpace shouldBe """{"programmes":["thisisprogramme1","thisisprogramme2"]}"""
  }

  it should "have a fromjson" in {
    implicitly[FromJson[EnrichedMostPopular]].apply("""["this is programme1","this is programme2"]""") shouldBe enrichedMostPopular

  }


}
