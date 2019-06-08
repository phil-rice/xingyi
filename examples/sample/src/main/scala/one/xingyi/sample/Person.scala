package one.xingyi.sample

import one.xingyi.core.optics.Lens

object Address{
  val addressLine1L: Lens[Address, String] = Lens(_.line1, (p, l) => p.copy(line1= l))
  val addressLine2L: Lens[Address, String] = Lens(_.line1, (p, l) => p.copy(line1= l))

}
case class Address(line1: String, line2: String)


object Person{
  val personAddresL : Lens[Person, Address] = Lens[Person,Address](_.address, (p, a) => p.copy(address=a))
  val personLine1L: Lens[Person, String] = Person.personAddresL andThen Address.addressLine1L
  val personLine2L: Lens[Person, String] = Person.personAddresL andThen Address.addressLine2L
  val p = Person("name",Address("myLine1", "myLine2"));

  println(personLine1L(p))

  val newP = personLine1L.set(p, "newName")

}
case class Person(name: String,address: Address) {

}

