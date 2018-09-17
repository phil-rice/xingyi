/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.reflection
import one.xingyi.core.UtilsSpec
import DefinedInSourceCodeAtLanguage._
class DefinedAtSpec extends UtilsSpec {

  behavior of "DefinedInSourceCodeAt"

  val s1 = DefinedInSourceCodeAt.definedInSourceCodeAt(1);
  val (s2a, s2b) = (DefinedInSourceCodeAt.definedInSourceCodeAt(1), DefinedInSourceCodeAt.definedInSourceCodeAt(1))
  it should "Allow the calculation of the line the call was made in" in {
    s1.st.toString shouldBe "one.xingyi.core.reflection.DefinedAtSpec.<init>(DefinedAtSpec.scala:9)"
  }

  implicit val isDefinedInSourceCodeAt: IsDefinedInSourceCodeAt[DefinedAtSpec] = _.s1

  it should "provide a pretty dump of the 'is defined at'" in {
    DefinedInSourceCodeAt(this).toString shouldBe "(DefinedAtSpec.scala:9)"
    this.definedInSourceCodeAt.toString shouldBe "(DefinedAtSpec.scala:9)"
  }

  it should "have an equals method" in {
    s1 shouldNot be(s2a)
    s2a should be(s2b)
    s1 shouldNot be("(DefinedAtSpec.scala:8)")
  }


  behavior of "CompositeDefinedInSourceCodeAt"

  it should "be created from the singles" in {
    val composite = CompositeDefinedInSourceCodeAt(Seq(s1, s2a, s2b))
    composite.toString shouldBe "Defined at[(DefinedAtSpec.scala:9),(DefinedAtSpec.scala:10),(DefinedAtSpec.scala:10)]"
  }
}
