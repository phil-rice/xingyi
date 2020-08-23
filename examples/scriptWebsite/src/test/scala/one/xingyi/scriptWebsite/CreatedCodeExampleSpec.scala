/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.xingyi.scriptExample

import one.xingyi.core.UtilsSpec
import one.xingyi.core.script.{IXingYi, IXingYiLoader}
import one.xingyi.scriptExample.createdCode1.{Person, PersonNameOps}

import scala.io.Source

class CreatedCodeExampleSpec extends UtilsSpec {

  val json = Source.fromInputStream(getClass.getResourceAsStream("/sample.json")).mkString
  behavior of "Example"

  def setup(fn: (IXingYi) => Unit): Unit = {
    val javascript = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("example.js")).mkString
    implicit val xingyi = implicitly[IXingYiLoader].apply(javascript)
    fn(xingyi)
  }
  it should "allow the person's name (lens and stringLens) to be extracted" in {
    setup { implicit xingyi =>
      val ops = new PersonNameOps
import ops._
      val person = xingyi.parse[Person](json)
      val person2 = nameLens.set(person, "New Name")
      val person3 = nameLens.set(person2, "Newer Name")

      nameLens.get(person) shouldBe "Phil Rice"
      nameLens.get(person2) shouldBe "New Name"
      nameLens.get(person3) shouldBe "Newer Name"
    }
  }
//  it should "allow the address to be extracted (listLens)" in {
//    setup { implicit xingyi =>
//      val personAddressOps = new PersonAddressListOps
//      val addressOps = new AddressOps
//
//      val person: Person = xingyi.parse[Person](json)
//      val addresses: List[Address] = personAddressOps.addressListLens.get(person)
//      addresses.map(addressOps.line1Lens.get) shouldBe List("No fixed abode", "A second address")
//    }
//  }
//
//  it should "allow the legacy person / line code to be used" in {
//    setup { implicit xingyi =>
//      val personLine1Ops = new PersonLine12Ops()
//
//      val person: Person = xingyi.parse[Person](json)
//      personLine1Ops.line1Lens.apply(person) shouldBe "No fixed abode"
//      val person1 = personLine1Ops.line1Lens.set(person, "new address")
//      personLine1Ops.line1Lens.apply(person) shouldBe "No fixed abode"
//      personLine1Ops.line1Lens.apply(person1) shouldBe "new address"
//    }
//  }
}
