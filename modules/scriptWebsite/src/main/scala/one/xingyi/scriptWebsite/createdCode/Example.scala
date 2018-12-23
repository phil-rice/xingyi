import one.xingyi.core.optics.Lens
import one.xingyi.core.script.{DomainMaker, IXingYi}
import one.xingyi.scriptShared._
class Person (mirror: Object) extends IPerson
object Person {
  implicit object default extends DomainMaker[Person] {
    override def create(mirror: Object): Person = Person(mirror)
  }
}
class Address (mirror: Object) extends IAddress
object Address {
  implicit object default extends DomainMaker[Address] {
    override def create(mirror: Object): Address = Address(mirror)
  }
}
class TelephoneNumber (mirror: Object) extends ITelephoneNumber
object TelephoneNumber {
  implicit object default extends DomainMaker[TelephoneNumber] {
    override def create(mirror: Object): TelephoneNumber = TelephoneNumber(mirror)
  }
}


class PersonNameOpsImpl(implicit xingYi: IXingYi) extends IPersonNameOps[Lens, Person]{
   def name = xingYi.stringLens[Person]("lens_person_name_string")
}
class PersonTelephoneOpsImpl(implicit xingYi: IXingYi) extends IPersonTelephoneOps[Lens, Person]{
   def telephoneNumber = xingYi.objectLens[Person,TelephoneNumber]("lens_person_telephonenumber_telephonenumber")
}
class PersonAddressListOpsImpl(implicit xingYi: IXingYi) extends IPersonAddressListOps[Lens, Person]{
   def addressList = xingYi.objectLens[Person,Address]("lens_person_addresses_addresslist")
}
class AddressOpsImpl(implicit xingYi: IXingYi) extends IAddressOps[Lens, Address]{
   def line1 = xingYi.stringLens[Address]("lens_address_line1_string")
   def line2 = xingYi.stringLens[Address]("lens_address_line2_string")
}
class TelephoneOpsImpl(implicit xingYi: IXingYi) extends ITelephoneNumberOps[Lens, TelephoneNumber]{
   def number = xingYi.stringLens[TelephoneNumber]("lens_telephonenumber_number_string")
}