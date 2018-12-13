package org.xingyi.script.server.createdCode

import one.xingyi.core.json.{JsonObject, JsonWriter, JsonWriterLangauge, ToJsonLib}
import one.xingyi.core.optics.Lens
import org.xingyi.script.server.Person.toListT
import org.xingyi.script.{Domain, DomainMaker, IXingYi, Payload}


case class Address(mirror: Object) extends Domain
object Address {
  implicit def AddressMaker: DomainMaker[Address] = Address.apply
}
case class Person(mirror: Object) extends Domain
object Person {
  implicit  def PersonMaker: DomainMaker[Person] = Person.apply
}


class   ExampleDomain(implicit xingYi: IXingYi) {
  def root: Lens[Payload, Person] = xingYi.objectLens[Payload, Person]("root")
  def person_name: Lens[Person, String] = xingYi.stringLens[Person]("person_name")
  def person_address: Lens[Person, Address] = xingYi.objectLens("person_address")
  def address_name: Lens[Address, String] = xingYi.stringLens("address_name")
}