/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.sample.domain
import one.xingyi.core.UtilsSpec
import one.xingyi.core.cache.{CachableKey, ObjectId}
import one.xingyi.core.http._
import one.xingyi.core.json._
import one.xingyi.core.monad.IdentityMonad

import scala.reflect.ClassTag
import scala.util.Success

class ProductionSpec[J: JsonWriter : JsonParser : ClassTag] extends UtilsSpec with DomainFixture {

  behavior of "ProductionId for" + implicitly[ClassTag[J]].runtimeClass.getSimpleName

  it should "have a tojson" in {
    implicitly[ToJson[ProductionId]].apply(productionId2) shouldBe """"2""""
  }

  it should "have a fromjsonlib - can't test the fromjson directly easily" in {
    val jString = implicitly[JsonWriter[J]].toJ(JsonString(productionId2.id))
    implicitly[FromJsonLib[J, ProductionId]].apply(jString) shouldBe productionId2
  }
  it should "have a ToServiceRequest" in {
    implicitly[ToServiceRequest[ProductionId]].apply(productionId2) shouldBe ServiceRequest(Get, Uri(s"/production/${productionId2.id}"))
  }

  it should "have a FromServiceRequest" in {
    implicitly[FromServiceRequest[IdentityMonad, ProductionId]].apply(ServiceRequest(Get, Uri("/something/id"))).value shouldBe Success(ProductionId("id", false))
  }

  it should "have a CachableKey" in {
    val key = implicitly[CachableKey[ProductionId]]
    key.id(productionId2) shouldBe ObjectId(productionId2)
    key.bypassCache(productionId2) shouldBe false
    key.bypassCache(productionId2.copy(bypassCache = true)) shouldBe true
  }

  behavior of "Production for" + implicitly[ClassTag[J]].runtimeClass.getSimpleName

  it should "have a fromJsonlib - cannot directly check fromjson" in {
    val jString = implicitly[JsonWriter[J]].toJ(JsonString("someProduction"))
    implicitly[FromJsonLib[J, Production]].apply(jString) shouldBe production2
  }

  it should "have a toJson" in {
    implicitly[ToJson[Production]].apply(production2).noWhiteSpace shouldBe """{"productionInfo":"someProduction"}"""
  }

}
