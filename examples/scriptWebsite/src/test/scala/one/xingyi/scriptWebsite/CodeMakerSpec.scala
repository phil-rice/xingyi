/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.scriptWebsite

import one.xingyi.core.CoreSpec
import one.xingyi.core.script._

import scala.io.Source

trait CodeMakerFixture {
  case class PayloadForTest(mirror: Object) extends Domain
  object PayloadForTest {
    implicit def domainmaker: DomainMaker[PayloadForTest] = PayloadForTest.apply
  }
}

class CodeMakerSpec extends CoreSpec with CodeMakerFixture {

  behavior of "Java script code maker"

  val json = Source.fromInputStream(getClass.getResourceAsStream("/sample.json")).mkString
  def setup(fn: IXingYi => Unit): Unit = {
    //    val codeMaker = implicitly[HasLensCodeMaker[Javascript]]
    val javascript = Source.fromResource("example.js").mkString
    val xingyi = implicitly[IXingYiLoader].apply(javascript)
    fn(xingyi)
  }

  it should "parse then render" in {
    setup { xingyi =>
      println("json: " + xingyi.render("pretty", xingyi.parse[PayloadForTest](json)))
    }

  }

  //This is an integration test that checks the javascript loaded along with IXingYi
  it should "allow the person's name to be extracted" in {
    setup { xingyi =>

      val j = xingyi.parse[PayloadForTest](json)
      println(json)

      val namesLens = xingyi.stringLens[PayloadForTest]("lens_person_name_string")
      namesLens.get(j) shouldBe "Phil Rice"
      val j1 = namesLens.set(j, "New Name")
      println("J  is " + j)
      println("J1 is " + j1)
      namesLens.get(j1) shouldBe "New Name"
      val j2 = namesLens.set(j1, "Newer Name")

      namesLens.get(j2) shouldBe "Newer Name"

      namesLens.get(j1) shouldBe "New Name"
      namesLens.get(j2) shouldBe "Newer Name"
    }
  }

  //  it should "make a scala class" in {
  //    val file = List("package org.xingyi.testing", implicitly[HasLensCodeMaker[ScalaDomain]].apply(exampleDomain), implicitly[HasLensCodeMaker[ScalaTrait]].apply(exampleDomain)).mkString("\n")
  //    println(file)
  //  }

  //  it should "make a string with a function for each lens" in {
  //    println
  //    println
  //    println(implicitly[HasLensCodeMaker[Javascript]].apply(new ExampleDomain))
  //    println
  //    println
  //    println(implicitly[HasLensCodeMaker[ScalaDomain]].apply(new ExampleDomain))
  //
  //    println(implicitly[HasLensCodeMaker[ScalaTrait]].apply(new ExampleDomain))
  //
  //  }

}
