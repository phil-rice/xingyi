package one.xingyi.sample.domain
import one.xingyi.core.UtilsSpec
import one.xingyi.core.cache.{CachableKey, ObjectId}
import one.xingyi.core.http.{Get, ServiceRequest, ToServiceRequest, Uri}
import one.xingyi.core.json._

import scala.reflect.ClassTag

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
    implicitly[ToJson[Production]].apply(production2) shouldBe """{productionInfo: "someProduction"}"""
  }

}
