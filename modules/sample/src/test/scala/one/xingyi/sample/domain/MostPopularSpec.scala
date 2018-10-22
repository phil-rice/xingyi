package one.xingyi.sample.domain
import java.security.Provider.Service

import one.xingyi.core.UtilsSpec
import one.xingyi.core.aggregate.{Enricher, HasChildren}
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
