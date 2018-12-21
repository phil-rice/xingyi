/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.scriptBackend

import one.xingyi.core.http._
import one.xingyi.core.json.{IXingYiLens, IXingYiSharedOps, StringFieldProjection, _}
import one.xingyi.core.monad.Monad
import one.xingyi.core.optics.Lens
import one.xingyi.core.script.DomainDefn
import one.xingyi.core.strings.Strings
import one.xingyi.scriptShared._
import scala.language.implicitConversions
import scala.language.higherKinds

case class Telephone(number: String)
object Telephone {
  val prototype = Telephone("prototype")
  implicit object telephoneProof extends ProofOfBinding[ITelephoneNumber, Telephone]
  implicit object telephoneOps extends ITelephoneNumberOps[IXingYiLens] {
    override def number = XingYiDomainStringLens.stringLens[ITelephoneNumber, Telephone](_.number, (t, n) => t.copy(number = n))
  }

  implicit val projection = ObjectProjection[Telephone](prototype, "number" -> StringFieldProjection[Telephone](_.number, (t, n) => t.copy(number = n)))
}

case class Address(line1: String, line2: String, postcode: String)

object Address extends JsonWriterLanguage {
  val prototype = Address("", "", "")
  val line1L = Lens[Address, String](_.line1, (a, s) => a.copy(line1 = s))
  val line2L = Lens[Address, String](_.line2, (a, s) => a.copy(line2 = s))

  implicit object addressProof extends ProofOfBinding[IAddress, Address]
  implicit object addressOps extends IAddressOps[IXingYiLens] {
    override def line1 = XingYiDomainStringLens(line1L)
    override def line2 = XingYiDomainStringLens.stringLens[IAddress, Address](_.line2, (a, s) => a.copy(line2 = s))
  }

  implicit val projection = ObjectProjection[Address](prototype,
    "line1" -> StringFieldProjection(addressOps.line1),
    "line2" -> StringFieldProjection(addressOps.line2),
    "postcode" -> StringFieldProjection(_.postcode, (a, s) => a.copy(postcode = s))
  )
}
case class Person(name: String, address: List[Address], telephoneNumber: Telephone)

object Person extends JsonWriterLanguage {
  implicit val nameL = Lens[Person, String](_.name, (p, n) => p.copy(name = n))
  implicit val personAddressListL = Lens[Person, List[Address]](_.address, (p, al) => p.copy(address = al))
  implicit val personAddressL = personAddressListL andThen Lens.firstItemL
  implicit val line1L = personAddressL andThen Address.line1L
  implicit val line2L = personAddressL andThen Address.line2L
  implicit val telephoneL = Lens[Person, Telephone](_.telephoneNumber, (p, t) => p.copy(telephoneNumber = t))

  implicit object personProof extends ProofOfBinding[IPerson, Person]
  implicit object personNameOps extends IPersonNameOps[IXingYiLens] {
    override def name = XingYiDomainStringLens(nameL)
  }
  implicit object personLine12Ops extends IPersonLine12Ops[IXingYiLens] {
    override def line1: IXingYiLens[IPerson, String] = XingYiDomainStringLens(personAddressL andThen Address.line1L)
    override def line2: IXingYiLens[IPerson, String] = XingYiDomainStringLens(personAddressL andThen Address.line2L)

  }
  implicit object personAddressOps extends IPersonAddressOps[IXingYiLens] {
    override def address: IXingYiLens[IPerson, IAddress] = XingYiDomainLens(personAddressL)
  }
  implicit object personAddressListOps extends IPersonAddressListOps[IXingYiLens] {
    override def addressList = XingYiDomainLens(personAddressListL)
  }

  implicit object personTelephoneOps extends IPersonTelephoneOps[IXingYiLens] {
    override def telephoneNumber = XingYiDomainLens(telephoneL)
  }
  val prototype = Person("", List(), Telephone.prototype)
  implicit val projection = ObjectProjection[Person](prototype,
    "name" -> StringFieldProjection(personNameOps.name),
    "telephoneNumber" -> ObjectFieldProjection(personTelephoneOps.telephoneNumber),
    "address" -> ListFieldProjection(personAddressListOps.addressList))
}


case class PersonRequest(name: String, xingYiHeader: Option[String])
object PersonRequest {
  implicit def fromServiceRequest[M[_]](implicit monad: Monad[M]): FromServiceRequest[M, PersonRequest] = { sr =>
    monad.liftM(PersonRequest(Strings.lastSection("/")(sr.uri.path.path), sr.header("xingyi")))
  }
}

case class EditPersonRequest(person: Person, xingYiHeader: Option[String]) // aha need to be able to make from projection
object EditPersonRequest {
  implicit def fromServiceRequest[M[_], J: JsonParser](implicit monad: Monad[M], projection: Projection[Person]): FromServiceRequest[M, EditPersonRequest] = { sr =>
    val name = Strings.lastSection("/")(sr.uri.path.path)
    val newPerson: Person = projection.fromJsonString(sr.body.getOrElse(throw new RuntimeException("cannot create person as body of request empty")).s)
    if (name != newPerson.name) throw new RuntimeException("Cannot edit name")
    monad.liftM(EditPersonRequest(newPerson, sr.header("xingyi")))
  }
}

class ExampleDomainDefn extends DomainDefn[Person](List("json", "pretty"), implicitly[ProjectionToLensDefns].apply(Person.projection)) {
  override def packageName: String = "one.xingyi.scriptExample.createdCode"
  override def domainName: String = "ExampleDomain"
}

case class NewExampleDomainDefn(list: IXingYiSharedOps[IXingYiLens, _]) {


  List[FromSharedToDomainForJavaScript[_, _]](
    Person.personNameOps -> Person.projection, // this says 'work it out from the projectíon' which must have the Person.projection inside it
    Person.personTelephoneOps -> Person.projection,
    Person.personAddressOps -> Person.projection,
    Address.addressOps -> Address.projection,
    Telephone.telephoneOps -> Telephone.projection,
    WorkOutJavascriptLensManually[IPerson, Person](Person.personLine12Ops, new IPersonLine12Ops[XingYiManualPath] {
      override def line1 = XingYiManualPath("legacy_person_line1_lens")
      override def line2 = XingYiManualPath("legacy_person_line1_lens")
    }),
    WorkOutJavascriptLensManually[IPerson, Person](Person.personAddressOps, new IPersonAddressOps[XingYiManualPath] {
      override def address = XingYiManualPath[IPerson, IAddress]("legacy_address")
    })
  )


}
