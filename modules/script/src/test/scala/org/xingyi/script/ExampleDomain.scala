package org.xingyi.script

import one.xingyi.core.json._

case class Address(line1: String, line2: String, postcode: String)
object Address extends JsonWriterLangauge {
  implicit def toJson[J: JsonWriter]: ToJsonLib[Address] =
    a => JsonObject("line1" -> a.line1, "line2" -> a.line2)

}

case class Person(name: String, address: List[Address])
object Person extends JsonWriterLangauge {
  implicit def toJson[J: JsonWriter](implicit address2Json: ToJsonLib[Address]): ToJsonLib[Person] =
    p => JsonObject("name" -> p.name, "address" -> toListT(p.address))
}


class ExampleDomain extends ScriptDomain {
  override def renderers: List[String] = List("json", "pretty")

  val personNameL = LensDefn[Person, String]("person_name", List("name"))
  val personAddressL = LensDefn[Person, Address]("person_address", List("address"))
  val addressLine1L = LensDefn[Address, String]("address_line1", List("line1"))
}
