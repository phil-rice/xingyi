/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.scriptBackend3

import one.xingyi.core.builder.{CopyWithNewId, HasId, IdLens}
import one.xingyi.core.json.{IXingYiLens, StringFieldProjection, _}
import one.xingyi.core.optics.Lens
import one.xingyi.core.script._
import one.xingyi.scriptModel3._

import scala.language.{higherKinds, implicitConversions}

case class Telephone(number: String)

object Telephone {
  val prototype = Telephone("prototype")

  implicit object telephoneProof extends ProofOfBinding[ITelephoneNumber, Telephone]

  implicit object telephoneOps extends ITelephoneNumberOps[IXingYiLens, ITelephoneNumber] {
    override def numberLens = XingYiDomainStringLens.stringLens[ITelephoneNumber, Telephone](_.number, (t, n) => t.copy(number = n))
  }

  val numberL: Lens[Telephone, String] = Lens(_.number, (t, n) => t.copy(number = n))
  implicit val projection = ObjectProjection[ITelephoneNumber, Telephone](prototype, "number" -> StringFieldProjection(telephoneOps.numberLens))
}

case class Address(line1: String, line2: String, postcode: String)

object Address extends JsonWriterLanguage {
  val prototype = Address("", "", "")

  implicit val links: Links[Address] = _ => List(LinkDetail("self", "/address/<id>"))
  implicit val entityPrefix: EntityPrefix[Address] = () => "address"

  val line1L = Lens[Address, String](_.line1, (a, s) => a.copy(line1 = s))
  val line2L = Lens[Address, String](_.line2, (a, s) => a.copy(line2 = s))

  implicit object addressProof extends ProofOfBinding[IAddress, Address]

  implicit object addressOps extends IAddressOps[IXingYiLens, IAddress] {
    override def line1Lens = XingYiDomainStringLens(line1L)

    override def line2Lens = XingYiDomainStringLens.stringLens[IAddress, Address](_.line2, (a, s) => a.copy(line2 = s))
  }

  implicit val projection = ObjectProjection[IAddress, Address](prototype,
    "line1" -> StringFieldProjection(addressOps.line1Lens),
    "line2" -> StringFieldProjection(addressOps.line2Lens),
    "postcode" -> StringField(Lens(_.postcode, (a, s) => a.copy(postcode = s)))
  )
}

case class Person(name: String, address: List[Address], telephoneNumber: Telephone)

object Person extends JsonWriterLanguage {
  implicit val entityPrefix: EntityPrefix[Person] = () => "person"
  implicit val links: Links[Person] = _ => List(LinkDetail("self", "/person/<id>"))

  implicit val idLens = IdLens[Person, String](_.name, (p, id) => p.copy(name = id))

  implicit object personProof extends ProofOfBinding[IPerson, Person]

  implicit object personNameOps extends IPersonNameOps[IXingYiLens, IPerson] {
    override def nameLens = XingYiDomainStringLens(Lens[Person, String](_.name, (p, n) => p.copy(name = n)))
  }

  implicit object personTelephoneOps extends IPersonTelephoneOps[IXingYiLens, IPerson, ITelephoneNumber] {
    override def telephoneNumberLens = XingYiDomainObjectLens(Lens[Person, Telephone](_.telephoneNumber, (p, t) => p.copy(telephoneNumber = t)))
  }

  implicit object personAddressListOps extends IPersonAddressListOps[IXingYiLens, IPerson, IAddress] {
    override def addressListLens = XingYiDomainObjectLens(Lens[Person, List[Address]](_.address, (p, a) => p.copy(address = a)))
  }

  val prototype = Person("", List(Address("someLine1", "someLine2", "somePostCode")), Telephone.prototype)
  implicit val projection = ObjectProjection[IPerson, Person](prototype,
    "name" -> StringFieldProjection(personNameOps.nameLens),
    "telephoneNumber" -> ObjectFieldProjection[IPerson, ITelephoneNumber, Person, Telephone](personTelephoneOps.telephoneNumberLens),
    //    "placesTheyLive" -> JsonHolder(  // work out how to do this
    "addresses" -> ListFieldProjection(personAddressListOps.addressListLens))


}

class Model3PersonDefn extends DomainDefn[IPerson, Person]("one.xingyi.scriptModel3", List("json", "pretty"),
  List(
    Person.personNameOps -> Person.projection,
    Person.personTelephoneOps -> Person.projection,
    Person.personAddressListOps -> Person.projection,
    Address.addressOps -> Address.projection,
    Telephone.telephoneOps -> Telephone.projection),
  List(
    new IPersonAddressOps[XingYiManualPath, IPerson, IAddress] {
      override def addressLens = XingYiManualPath[IPerson, IAddress]("lens_person_address_address", "objectLens",
        """function lens_person_address_address() { return compose(lens_person_addresses_addresslist(), lensForFirstItemInList())}""", isList = true)
    },
    new IPersonLine12Ops[XingYiManualPath, IPerson] {
      override val line1Lens = XingYiManualPath[IPerson, String]("lens_person_line1_string", "stringLens",
        """function lens_person_line1_string() { return compose(lens_person_address_address(), lens("line1"))}""")

      override def line2Lens = XingYiManualPath[IPerson, String]("lens_person_line2_string", "stringLens",
        """function lens_person_line2_string() { return compose(lens_person_address_address(), lens("line2"))}""")
    })) {
  override def packageName: String = "one.xingyi.scriptExample.createdCode"

}

class Model3AddressDefn extends DomainDefn[IAddress, Address]("one.xingyi.scriptModel3", List("json", "pretty"),
  List(Address.addressOps -> Address.projection),
  List()) {
  override def packageName: String = "one.xingyi.scriptExample.createdCode"

  override def domainName: String = "Address"
}


object TestItQuick2 extends App {
  //  val x: ToScalaCode[IXingYiLensAndLensDefn] => ToScalaCode[InterfaceAndLens[Any, Any]] = ToScalaCode.makeScaleForInterface[Any, Any]
  //  ToScalaCode.makeScaleForInterface[Any, Any]
  //  val x = ToScalaCode.makeScalaCode[DomainDefn[Person]]
  val makeScala = implicitly[ToScalaCode[DomainDefn[IPerson, Person]]]
  println(makeScala(new Model3PersonDefn))
}