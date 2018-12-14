package org.xingyi.script

import one.xingyi.core.json._

case class Address(line1: String, line2: String, postcode: String)

object Address extends JsonWriterLangauge {
  implicit def toJsonV1[J: JsonWriter]: ToJsonLib[Address] =
    a => JsonObject("line1" -> a.line1, "line2" -> a.line2)
  implicit def toJsonV2[J: JsonWriter]: ToJsonLib[Address] =
    a => JsonObject("lineone" -> a.line1, "lineone" -> a.line2)

}

case class Person(name: String, address: List[Address], telephoneNumber: String)

object Person extends JsonWriterLangauge {
  implicit def toJson[J: JsonWriter](implicit address2Json: ToJsonLib[Address]): ToJsonLib[Person] =
    p => JsonObject("name" -> p.name, "addressList" -> JsonObject("address" -> toListT(p.address)), "telephoneNumber" -> p.telephoneNumber)
}


class ExampleDomain extends ScriptDomain {
  override def renderers: List[String] = List("json", "pretty")

  val personNameL = LensDefn[Person, String]("person_name", List("name"))
  val personAddressL = LensDefn[Person, Address]("person_address", List("addresslist","address"))
  val addressLine1L = LensDefn[Address, String]("address_line1", List("line1"))
  val addressLine2L = LensDefn[Address, String]("address_line2", List("line2"))
  val persontelL = LensDefn[Person, String]("person_telephone", List("telephoneNumber"))
}
