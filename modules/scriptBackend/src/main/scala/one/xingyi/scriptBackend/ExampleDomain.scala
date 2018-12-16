/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.scriptBackend

import one.xingyi.core.http._
import one.xingyi.core.json._
import one.xingyi.core.monad.Monad
import one.xingyi.core.strings.Strings
import org.xingyi.script._

case class Telephone(number: String)
object Telephone {
  val prototype = Telephone("prototype")
  implicit val project = ObjectProjection[Telephone](prototype, "number" -> StringFieldProjection[Telephone](_.number, (t, n) => t.copy(number = n)))
}

case class Address(line1: String, line2: String, postcode: String)

object Address extends JsonWriterLangauge {
  //Note that I can change this to reflection or a macro for the happy case
  implicit val projection = ObjectProjection[Address](Address("", "", ""),
    "line1" -> StringFieldProjection(_.line1, (a, l1) => a.copy(line1 = l1)),
    "line2" -> StringFieldProjection(_.line2, (a, l2) => a.copy(line2 = l2)),
    "postcode" -> StringFieldProjection(_.postcode, (a, pc) => a.copy(postcode = pc))
  )
}
case class Person(name: String, address: List[Address], telephoneNumber: Telephone)

object Person extends JsonWriterLangauge {
  val prototype = Person("", List(), Telephone.prototype)
  implicit val projection = ObjectProjection[Person](prototype,
    "name" -> StringFieldProjection(_.name, (p, n) => p.copy(name = n)),
    "telephoneNumber" -> ObjectFieldProjection[Person, Telephone](_.telephoneNumber, (p, a) => p.copy(telephoneNumber = a)),
    "address" -> ListFieldProjection[Person, Address](_.address, (p, list) => p.copy(address = list))
  )

  implicit def toJson[J](implicit projection: ObjectProjection[Person]): ToJsonLib[Person] = person => projection.toJson(person)

}


case class PersonRequest(name: String)
object PersonRequest {
  implicit def fromServiceRequest[M[_]](implicit monad: Monad[M]): FromServiceRequest[M, PersonRequest] = { sr =>
    monad.liftM(PersonRequest(Strings.lastSection("/")(sr.uri.path.path)))
  }
}

case class EditPersonRequest( person: Person) // aha need to be able to make from projection
object EditPersonRequest {
  implicit def fromServiceRequest[M[_], J: JsonParser](implicit monad: Monad[M], projection: Projection[Person]): FromServiceRequest[M, EditPersonRequest] = { sr =>
    val name = Strings.lastSection("/")(sr.uri.path.path)
    val newPerson: Person = projection.fromJsonString(sr.body.getOrElse(throw new RuntimeException("cannot create person as body of request empty")).s)
    if (name != newPerson) throw new RuntimeException("Cannot edit name")
    monad.liftM(EditPersonRequest(newPerson))
  }

}


class ExampleDomain extends ScriptDomain {
  override def renderers: List[String] = List("json", "pretty")
  override def lens: Seq[LensDefn[_, _]] = implicitly[ProjectionToLensDefns].apply(Person.projection)
}
