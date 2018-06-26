/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.reflection

import one.xingyi.core.UtilsSpec

import scala.reflect.ClassTag

class ClassTagsSpec extends UtilsSpec {


  "ClassTags.clazz" should "return the class of the parameter" in {
    ClassTags.clazz[String] shouldBe classOf[String]
  }
  "ClassTags.nameOf" should "return the name of the parameter" in {
    ClassTags.nameOf[String] shouldBe "String"

  }
  "ClassTags.isA" should "return true if the parameter 'isa'" in {
    ClassTags.isA[String]("") shouldBe true
    ClassTags.isA[Int]("") shouldBe false

  }
  "ClassTags.collectAll" should "return all of the items in the collection that 'isa'" in {
    ClassTags.collectAll[Number](Seq(1, 1.0, "1")) shouldBe Seq(1, 1.0)
  }

  import ClassTags._

  "AnyPimperForClassTags.is" should "return true if the parameter 'isa'" in {
    "".is[String] shouldBe true
    1.is[String] shouldBe false
  }
  "IterablePimperForClassTags.collectAs" should "return all of the items in the collection that 'isa'" in {
    (Seq(1, 1.0, "1")).collectAs[Number] shouldBe Seq(1, 1.0)

  }

}
