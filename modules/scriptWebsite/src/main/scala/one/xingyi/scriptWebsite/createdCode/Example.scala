package one.xingyi.scriptExample.createdCode
/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */

import one.xingyi.core.optics.Lens
import one.xingyi.core.script.{Domain, DomainMaker, IXingYi, Payload}


case class Person(mirror: Object) extends Domain
object Person {
   implicit def PersonMaker: DomainMaker[Person] = Person.apply
}
case class Address(mirror: Object) extends Domain
object Address {
   implicit def AddressMaker: DomainMaker[Address] = Address.apply
}
case class Telephone(mirror: Object) extends Domain
object Telephone {
   implicit def TelephoneMaker: DomainMaker[Telephone] = Telephone.apply
}

class ExampleDomain(implicit xingYi:IXingYi) {
   def header: String = "application/xingyi.address_line1_string.address_line1_string.address_line2_string.address_line2_string.address_postcode_string.address_postcode_string.person_address.person_addresses_addresslist.person_line1.person_line2.person_name_string.person_telephonenumber_telephonenumber.telephonenumber_number_string.telephonenumber_number_string"
   def payload(json: String): Payload = xingYi.parse(json)
   def root: Lens[Payload,Person] = xingYi.objectLens("root")
   def renderjson(domain: Domain): String = xingYi.render("json",domain)
   def renderpretty(domain: Domain): String = xingYi.render("pretty",domain)
   def person_name_string: Lens[IPerson,String] = xingYi.stringLens("person_name_string") 
   def person_telephonenumber_telephonenumber: Lens[IPerson,ITelephoneNumber] = xingYi.objectLens("person_telephonenumber_telephonenumber") 
   def telephonenumber_number_string: Lens[ITelephoneNumber,String] = xingYi.stringLens("telephonenumber_number_string") 
   def person_addresses_addresslist: Lens[IPerson,List[IAddress]] = xingYi.listLens("person_addresses_addresslist") 
   def address_line1_string: Lens[IAddress,String] = xingYi.stringLens("address_line1_string") 
   def address_line2_string: Lens[IAddress,String] = xingYi.stringLens("address_line2_string") 
   def address_postcode_string: Lens[IAddress,String] = xingYi.stringLens("address_postcode_string") 
   def address_line1_string: Lens[IAddress,String] = xingYi.stringLens("address_line1_string") 
   def address_line2_string: Lens[IAddress,String] = xingYi.stringLens("address_line2_string") 
   def address_postcode_string: Lens[IAddress,String] = xingYi.stringLens("address_postcode_string") 
   def telephonenumber_number_string: Lens[ITelephoneNumber,String] = xingYi.stringLens("telephonenumber_number_string") 
   def person_line1: Lens[IPerson,String] = xingYi.stringLens("person_line1") 
   def person_line2: Lens[IPerson,String] = xingYi.stringLens("person_line2") 
   def person_address: Lens[IPerson,List[IAddress]] = xingYi.listLens("person_address") 
}