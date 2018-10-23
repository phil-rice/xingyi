package one.xingyi.sample.domain

import one.xingyi.core.UtilsSpec
import one.xingyi.core.cache.{CachableKey, ObjectId}
import one.xingyi.core.http._
import one.xingyi.core.json._
import one.xingyi.core.monad.IdentityMonad

import scala.reflect.ClassTag
import scala.util.Success

class ProgrammeSpec[J: JsonWriter : JsonParser : ClassTag] extends UtilsSpec with DomainFixture {

  behavior of "ProgrammeId for" + implicitly[ClassTag[J]].runtimeClass.getSimpleName

  it should "have a fromjsonlib - can't test the fromjson directly easily" in {
    val jString = implicitly[JsonWriter[J]].toJ(JsonString(programmeId1.id))
    implicitly[FromJsonLib[J, ProgrammeId]].apply(jString) shouldBe programmeId1
  }
  it should "have a ToServiceRequest" in {
    implicitly[ToServiceRequest[ProgrammeId]].apply(programmeId1) shouldBe ServiceRequest(Get, Uri(s"/programme/${programmeId1.id}"))
  }

  it should "have a FromServiceRequest" in {
    implicitly[FromServiceRequest[IdentityMonad, ProgrammeId]].apply(ServiceRequest(Get, Uri("/something/id"))).value shouldBe Success(ProgrammeId("id", false))
  }

  it should "have a CachableKey" in {
    val key = implicitly[CachableKey[ProgrammeId]]
    key.id(programmeId1) shouldBe ObjectId(programmeId1)
    key.bypassCache(programmeId1) shouldBe false
    key.bypassCache(programmeId1.copy(bypassCache = true)) shouldBe true
  }

  behavior of "Programme for" + implicitly[ClassTag[J]].runtimeClass.getSimpleName

  it should "have a fromJsonlib - cannot directly check fromjson" in {
    val jString = implicitly[JsonWriter[J]].toJ(JsonString("this is programme1"))
    implicitly[FromJsonLib[J, Programme]].apply(jString) shouldBe programme1
  }

  it should "have a toJson" in {
    implicitly[ToJson[Programme]].apply(programme1).noWhiteSpace shouldBe """{programmeInfo:"thisisprogramme1"}"""
  }

}
