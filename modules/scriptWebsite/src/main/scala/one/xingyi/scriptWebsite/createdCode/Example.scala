package one.xingyi.scriptExample.createdCode


import one.xingyi.scriptShared._
import one.xingyi.core.optics.Lens
import one.xingyi.core.script.{Domain,DomainMaker, IXingYi}


case class Person (mirror: Object) extends Domain with IPerson
object Person {
  implicit object default extends DomainMaker[Person] {
    override def create(mirror: Object): Person = Person(mirror)
  }
}
case class Address (mirror: Object) extends Domain with IAddress
object Address {
  implicit object default extends DomainMaker[Address] {
    override def create(mirror: Object): Address = Address(mirror)
  }
}
case class TelephoneNumber (mirror: Object) extends Domain with ITelephoneNumber
object TelephoneNumber {
  implicit object default extends DomainMaker[TelephoneNumber] {
    override def create(mirror: Object): TelephoneNumber = TelephoneNumber(mirror)
  }
}


class PersonNameOps(implicit val xingYi: IXingYi) extends IPersonNameOps[Lens, Person]{
   def nameLens = xingYi.stringLens[Person]("lens_person_name_string")
}
class PersonTelephoneOps(implicit val xingYi: IXingYi) extends IPersonTelephoneOps[Lens, Person,TelephoneNumber]{
   def telephoneNumberLens = xingYi.objectLens[Person,TelephoneNumber]("lens_person_telephonenumber_telephonenumber")
}
class PersonAddressListOps(implicit val xingYi: IXingYi) extends IPersonAddressListOps[Lens, Person,Address]{
   def addressListLens = xingYi.listLens[Person,Address]("lens_person_addresses_addresslist")
}
class AddressOps(implicit val xingYi: IXingYi) extends IAddressOps[Lens, Address]{
   def line1Lens = xingYi.stringLens[Address]("lens_address_line1_string")
   def line2Lens = xingYi.stringLens[Address]("lens_address_line2_string")
}
class TelephoneNumberOps(implicit val xingYi: IXingYi) extends ITelephoneNumberOps[Lens, TelephoneNumber]{
   def numberLens = xingYi.stringLens[TelephoneNumber]("lens_telephonenumber_number_string")
}


class PersonAddressOps(implicit val xingYi: IXingYi) extends IPersonAddressOps[Lens, Person,Address] {//class one.xingyi.scriptBackend.ExampleDomainDefn$$anon$1
 def addressLens = xingYi.objectLens[Person,Address]("legacy_address") 
}
class PersonLine12Ops(implicit val xingYi: IXingYi) extends IPersonLine12Ops[Lens, Person] {//class one.xingyi.scriptBackend.ExampleDomainDefn$$anon$2
 def line1Lens = xingYi.stringLens[Person]("legacy_person_line1_lens") 
 def line2Lens = xingYi.stringLens[Person]("legacy_person_line1_lens") 
}