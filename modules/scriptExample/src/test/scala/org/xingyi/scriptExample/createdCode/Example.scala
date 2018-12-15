package org.xingyi.scriptExample.createdCode

import one.xingyi.core.optics.Lens
import org.xingyi.script.{Domain, DomainMaker, IXingYi, Payload}

case class Person(mirror: Object) extends Domain
object Person {
   implicit def PersonMaker: DomainMaker[Person] = Person.apply
}
case class Address(mirror: Object) extends Domain
object Address {
   implicit def AddressMaker: DomainMaker[Address] = Address.apply
}

class ExampleDomain(implicit xingYi:IXingYi) {
   def payload(json: String): Payload = xingYi.parse(json)
   def root: Lens[Payload,Person] = xingYi.objectLens("root")
   def renderjson(domain: Domain): String = xingYi.render("json",domain)
   def renderpretty(domain: Domain): String = xingYi.render("pretty",domain)
   def person_name: Lens[Person,String] = xingYi.stringLens("person_name") 
   def person_address_list: Lens[Person,List[Address]] = xingYi.listLens("person_address_list") 
   def address_line2: Lens[Address,String] = xingYi.stringLens("address_line2") 
   def person_telephoneNumber: Lens[Person,String] = xingYi.stringLens("person_telephoneNumber") 
   def address_line1: Lens[Address,String] = xingYi.stringLens("address_line1") 
}