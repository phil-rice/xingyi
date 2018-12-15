package org.xingyi.script

import one.xingyi.core.UtilsSpec
import one.xingyi.core.json.ObjectProjection

class ExampleDomainScriptDomainSpec extends UtilsSpec {

  val tel = Telephone("someNumber")
  val address1 = Address("line1", "line2", "pc1")
  val address2 = Address("line2", "line2", "pc2")
  val person = Person("someName", List(address1, address2), tel)

  behavior of "Example Domain turned into a sequence of LensDefns using projections"

  it should "make some lens defns" in {
    implicitly[ProjectionToLensDefns].apply(Person.projection).toSet shouldBe Set(
      LensDefn("person_name", List("name"), false),
      LensDefn("person_telephone", List("telephoneNumber"), false),
      LensDefn("telephone_number", List("number"), false),
      LensDefn("person_address_list", List("address"), true),
      LensDefn("address_line1", List("line1"), false),
      LensDefn("address_line2", List("line2"), false))
  }


}
