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
