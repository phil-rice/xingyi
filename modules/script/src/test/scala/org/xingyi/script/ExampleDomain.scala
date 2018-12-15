/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.xingyi.script

import one.xingyi.core.json
import one.xingyi.core.json._

case class Telephone(number: String)
object Telephone {
  implicit val project = ObjectProjection[Telephone]("number" -> StringFieldProjection(_.number))
}

case class Address(line1: String, line2: String, postcode: String)

object Address extends JsonWriterLangauge {
  //Note that I can change this to reflection or a macro for the happy case
  implicit val projection = ObjectProjection[Address](
    "line1" -> StringFieldProjection(_.line1),
    "line2" -> StringFieldProjection(_.line2)
  )
}

case class Person(name: String, address: List[Address], telephoneNumber: Telephone)

object Person extends JsonWriterLangauge {
  implicit val projection = ObjectProjection[Person](
    "name" -> StringFieldProjection(_.name),
    "telephoneNumber" -> ObjectFieldProjection[Person, Telephone](_.telephoneNumber),
    "address" -> ListFieldProjection[Person, Address](_.address)
  )
}


class ExampleDomain extends ScriptDomain {
  override def renderers: List[String] = List("json", "pretty")
  override def lens: Seq[LensDefn[_, _]] = implicitly[ProjectionToLensDefns].apply(Person.projection)

//  val personNameL = LensDefn.string[Person]("name")
//  val personAddressL = LensDefn.list[Person, Address]("address")
//  val addressLine1L = LensDefn.string[Address]("line1")
//  val addressLine2L = LensDefn.string[Address]("line2")
//  val persontelL = LensDefn.string[Person]("telephoneNumber")
}
