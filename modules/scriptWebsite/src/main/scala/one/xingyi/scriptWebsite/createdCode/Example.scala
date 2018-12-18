package one.xingyi.scriptExample.createdCode
/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */

import one.xingyi.core.json.ToJson
import one.xingyi.core.optics.Lens
import one.xingyi.core.script.{Domain, DomainMaker, IXingYi, Payload}


case class Person(mirror: Object) extends Domain
object Person {
   implicit def PersonMaker: DomainMaker[Person] = Person.apply
}

  //what if it is it's own resouce. Answer all the domains should be generated together. We can do that in the future.
case class Telephone(mirror: Object) extends Domain
object Telephone {
   implicit def TelephoneMaker: DomainMaker[Telephone] = Telephone.apply
}
case class Address(mirror: Object) extends Domain
object Address {
   implicit def AddressMaker: DomainMaker[Address] = Address.apply
}

class ExampleDomain(implicit xingYi:IXingYi) {
   def header: String = "application/xingyi.address_line1.address_line2.address_postcode.person_address_list.person_name.person_telephone.telephone_number"
   def payload(json: String): Payload = xingYi.parse(json)
   def root: Lens[Payload,Person] = xingYi.objectLens("root")
   def renderjson(domain: Domain): String = xingYi.render("json",domain)
   def renderpretty(domain: Domain): String = xingYi.render("pretty",domain)
   def person_name: Lens[Person,String] = xingYi.stringLens("person_name") 
   def person_telephone: Lens[Person,Telephone] = xingYi.objectLens("person_telephone") 
   def telephone_number: Lens[Telephone,String] = xingYi.stringLens("telephone_number") 
   def person_address_list: Lens[Person,List[Address]] = xingYi.listLens("person_address_list") 
   def address_line1: Lens[Address,String] = xingYi.stringLens("address_line1") 
   def address_line2: Lens[Address,String] = xingYi.stringLens("address_line2") 
   def address_postcode: Lens[Address,String] = xingYi.stringLens("address_postcode") 
}