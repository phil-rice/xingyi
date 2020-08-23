/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
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
