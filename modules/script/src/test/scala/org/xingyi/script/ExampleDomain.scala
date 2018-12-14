package org.xingyi.script

import one.xingyi.core.json._

case class Address(line1: String, line2: String, postcode: String)

object Address extends JsonWriterLangauge {
  implicit def toJson[J: JsonWriter]: ToJsonLib[Address] =
    a => JsonObject("line1" -> a.line1, "line2" -> a.line2)

}

case class Person(name: String, address: List[Address], telephoneNumber: String)

object Person extends JsonWriterLangauge {
  implicit def toJson[J: JsonWriter](implicit address2Json: ToJsonLib[Address]): ToJsonLib[Person] =
    p => JsonObject("name" -> p.name, "address" -> toListT(p.address), "telephoneNumber" -> p.telephoneNumber)
}


class ExampleDomain extends ScriptDomain {
  override def renderers: List[String] = List("json", "pretty")

  val personNameL = LensDefn.string[Person, String]("name")
  val personAddressL = LensDefn.list[Person, Address]("address")
  val addressLine1L = LensDefn.string[Address, String]("line1")
  val addressLine2L = LensDefn.string[Address, String]("line2")
  val persontelL = LensDefn.string[Person, String]("telephoneNumber")
}
