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
   def header: String = "application/xingyi.iaddress_line1.iaddress_line1.iaddress_line2.iaddress_line2.iaddress_postcode.iaddress_postcode.iperson_iaddress_list.iperson_itelephonenumber.iperson_name.itelephonenumber_number.itelephonenumber_number.person_address.person_line1.person_line2"
   def payload(json: String): Payload = xingYi.parse(json)
   def root: Lens[Payload,Person] = xingYi.objectLens("root")
   def renderjson(domain: Domain): String = xingYi.render("json",domain)
   def renderpretty(domain: Domain): String = xingYi.render("pretty",domain)
   def iperson_name: Lens[IPerson,String] = xingYi.stringLens("iperson_name") 
   def iperson_itelephonenumber: Lens[IPerson,ITelephoneNumber] = xingYi.objectLens("iperson_itelephonenumber") 
   def itelephonenumber_number: Lens[ITelephoneNumber,String] = xingYi.stringLens("itelephonenumber_number") 
   def iperson_iaddress_list: Lens[IPerson,List[IAddress]] = xingYi.listLens("iperson_iaddress_list") 
   def iaddress_line1: Lens[IAddress,String] = xingYi.stringLens("iaddress_line1") 
   def iaddress_line2: Lens[IAddress,String] = xingYi.stringLens("iaddress_line2") 
   def iaddress_postcode: Lens[IAddress,String] = xingYi.stringLens("iaddress_postcode") 
   def iaddress_line1: Lens[IAddress,String] = xingYi.stringLens("iaddress_line1") 
   def iaddress_line2: Lens[IAddress,String] = xingYi.stringLens("iaddress_line2") 
   def iaddress_postcode: Lens[IAddress,String] = xingYi.stringLens("iaddress_postcode") 
   def itelephonenumber_number: Lens[ITelephoneNumber,String] = xingYi.stringLens("itelephonenumber_number") 
   def person_line1: Lens[IPerson,String] = xingYi.stringLens("person_line1") 
   def person_line2: Lens[IPerson,String] = xingYi.stringLens("person_line2") 
   def person_address: Lens[IPerson,List[IAddress]] = xingYi.listLens("person_address") 
}