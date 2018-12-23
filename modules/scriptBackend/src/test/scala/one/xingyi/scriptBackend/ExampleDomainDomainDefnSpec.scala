/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.scriptBackend
import one.xingyi.core.UtilsSpec
import one.xingyi.core.json.{LensDefn, ProjectionToLensDefns, SimpleLensDefn}

class ExampleDomainDomainDefnSpec extends UtilsSpec {

  val tel = Telephone("someNumber")
  val address1 = Address("line1", "line2", "pc1")
  val address2 = Address("line2", "line2", "pc2")
  val person = Person("someName", List(address1, address2), tel)

  behavior of "Example Domain turned into a sequence of LensDefns using projections"

  it should "make some lens defns" in {
    fail()
//    implicitly[ProjectionToLensDefns].apply(Person.projection).sortBy(_.name) shouldBe List(
//      SimpleLensDefn("address_line1", List("line1"), false),
//      SimpleLensDefn("address_line2", List("line2"), false),
//      SimpleLensDefn("address_postcode", List("postcode"), false),
//      SimpleLensDefn("person_address_list", List("addresses"), true),
//      SimpleLensDefn("person_name", List("name"), false),
//      SimpleLensDefn("person_telephone", List("telephoneNumber"), false),
//      SimpleLensDefn("telephone_number", List("number"), false)
//    )
  }


}
