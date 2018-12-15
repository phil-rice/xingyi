package org.xingyi.script
import one.xingyi.core.UtilsSpec
import one.xingyi.core.json.{JsonWriter, ObjectProjection}

abstract class ExampleDomainToJsonSpec[J](implicit writer: JsonWriter[J]) extends UtilsSpec {

  val tel = Telephone("someNumber")
  val address1 = Address("line1", "line2", "pc1")
  val address2 = Address("line2", "line2", "pc2")
  val person = Person("someName", List(address1, address2), tel)
  behavior of "Example Domain ToJson using projections"

  it should "use the projection to turn a person into json" in {
    writer(implicitly[ObjectProjection[Person]].toJson(person)).noWhiteSpace shouldBe
    """{
      |  "name":"someName",
      |  "telephoneNumber":{
      |    "number":"someNumber"
      |  },
      |  "address":[{
      |    "line1":"line1",
      |    "line2":"line2"
      |  },{
      |    "line1":"line2",
      |    "line2":"line2"
      |  }]
      |}""".stripMargin.noWhiteSpace
  }
}
