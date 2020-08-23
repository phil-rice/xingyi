/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.script
import one.xingyi.core.UtilsSpec
import org.mockito.Mockito._
class DomainDetailsTest extends UtilsSpec with ScriptFixture {

  val domainForTest = new ParentDomainForTest1

  behavior of "DomainDetails"

  it should "be created from a domain defn" in {
    implicit val javascript = mock[HasLensCodeMaker[Javascript]]
    when(javascript.apply(domainForTest)) thenReturn "some Javascript code"
    implicit val scala = mock[ToScalaCode[DomainDefn[IParent, ParentForTest]]]
    when(scala.apply(domainForTest)) thenReturn "some Scala code"
    val details = implicitly[DomainDefnToDetails[IParent, ParentForTest]] apply domainForTest


    details.name shouldBe "ParentDomainForTest1"
    details.packageName shouldBe "one.xingyi.core.script"
    details.code shouldBe Map(Javascript -> CodeDetails("some Javascript code"), ScalaCode -> CodeDetails("some Scala code"))
    details.accept shouldBe "application/xingyi.lens_child_name_string,lens_house_postcode_string,lens_parent_children_childlist,lens_parent_house_house,lens_parent_name_string"
    details.renderers shouldBe List("renderer1", "renderer2")
    details.lensNames shouldBe Set("lens_house_postcode_string", "lens_parent_house_house", "lens_parent_children_childlist", "lens_parent_name_string", "lens_child_name_string")
  }

  it should "have a normalisedLens" in {
    val details = implicitly[DomainDefnToDetails[IParent, ParentForTest]] apply domainForTest
    details.normalisedLens shouldBe "lens_child_name_string,lens_house_postcode_string,lens_parent_children_childlist,lens_parent_house_house,lens_parent_name_string"
  }

  it should "be defined at if only include supported lens" in {
    val details = implicitly[DomainDefnToDetails[IParent, ParentForTest]] apply domainForTest

    details.isDefinedAt(Set()) shouldBe true
    details.isDefinedAt(Set("lens_parent_children_childlist", "lens_parent_house_house", "lens_parent_name_string")) shouldBe true
    details.isDefinedAt(Set("lens_child_name_string", "lens_house_postcode_string", "lens_parent_children_childlist", "lens_parent_house_house", "lens_parent_name_string")) shouldBe true

    details.isDefinedAt(Set("another", "lens_child_name_string", "lens_house_postcode_string", "lens_parent_children_childlist", "lens_parent_house_house", "lens_parent_name_string")) shouldBe false
    details.isDefinedAt(Set("another")) shouldBe false

  }
}
