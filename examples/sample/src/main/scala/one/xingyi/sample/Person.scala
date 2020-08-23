/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
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

