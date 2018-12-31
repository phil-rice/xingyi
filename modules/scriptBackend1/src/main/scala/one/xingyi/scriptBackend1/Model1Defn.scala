/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.scriptBackend1

import one.xingyi.core.builder.HasId
import one.xingyi.core.json.{IXingYiLens, StringFieldProjection, _}
import one.xingyi.core.optics.Lens
import one.xingyi.core.script._
import one.xingyi.scriptModel1._

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

case class Person(name: String, line1: String, line2: String, telephoneNumber: Telephone)

object Person extends JsonWriterLanguage {
  implicit val hasId: HasId[Person, String] = _.name

  implicit val links: Links[Person] = _ => List(LinkDetail("self", "/person/<id>"))

  implicit val nameL = Lens[Person, String](_.name, (p, n) => p.copy(name = n))
  implicit val line1L = Lens[Person, String](_.line1, (p, n) => p.copy(line1 = n))
  implicit val line2L = Lens[Person, String](_.line2, (p, n) => p.copy(line2 = n))
  implicit val telephoneL = Lens[Person, Telephone](_.telephoneNumber, (p, t) => p.copy(telephoneNumber = t))

  implicit object personProof extends ProofOfBinding[IPerson, Person]

  implicit object personNameOps extends IPersonNameOps[IXingYiLens, IPerson] {
    override def nameLens = XingYiDomainStringLens(nameL)
  }

  implicit object personLine12Ops extends IPersonLine12Ops[IXingYiLens, IPerson] {
    override def line1Lens = XingYiDomainStringLens(line1L)

    override def line2Lens = XingYiDomainStringLens(line2L)

  }

  implicit object personTelephoneOps extends IPersonTelephoneOps[IXingYiLens, IPerson, ITelephoneNumber] {
    override def telephoneNumberLens = XingYiDomainObjectLens(telephoneL)
  }

  val prototype = Person("", "", "", Telephone.prototype)
  implicit val projection = ObjectProjection[IPerson, Person](prototype,
    "name" -> StringFieldProjection(personNameOps.nameLens),
    "line1" -> StringFieldProjection[IPerson, Person](personLine12Ops.line1Lens),
    "line2" -> StringFieldProjection(personLine12Ops.line2Lens),
    "telephoneNumber" -> ObjectFieldProjection[IPerson, ITelephoneNumber, Person, Telephone](personTelephoneOps.telephoneNumberLens))
}


class Model1Defn extends DomainDefn[Person]("one.xingyi.scriptModel1", List("json", "pretty"),
  List(
    Person.personNameOps -> Person.projection,
    Person.personTelephoneOps -> Person.projection,
    Person.personLine12Ops -> Person.projection,
    Telephone.telephoneOps -> Telephone.projection),
  List()) {
  override def packageName: String = "one.xingyi.scriptExample.createdCode1"

  override def domainName: String = "Model1Domain"
}


object TestItQuick extends App {
  //  val x: ToScalaCode[IXingYiLensAndLensDefn] => ToScalaCode[InterfaceAndLens[Any, Any]] = ToScalaCode.makeScaleForInterface[Any, Any]
  //  ToScalaCode.makeScaleForInterface[Any, Any]
  //  val x = ToScalaCode.makeScalaCode[DomainDefn[Person]]
  val makeScala = implicitly[ToScalaCode[DomainDefn[Person]]]
  println(makeScala(new Model1Defn))
}