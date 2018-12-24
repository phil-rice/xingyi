/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.xingyi.scriptExample

import one.xingyi.core.UtilsSpec
import one.xingyi.core.script.{IXingYi, IXingYiLoader}
import one.xingyi.scriptExample.createdCode.{Person, PersonNameOpsImpl}
import one.xingyi.scriptShared.IPersonNameOps

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
      val ops = new PersonNameOpsImpl
      import ops._
      val person = xingyi.parse[Person](json)

      ops.nameLens.get(person) shouldBe "Phil Rice"

      val payload1 = nameLens.set(person, "New Name")
      nameLens.get(payload1) shouldBe "New Name"

      val payload2 = nameLens.set(payload1, "Newer Name")
      nameLens.get(payload2) shouldBe "Newer Name"

    }
  }
  //  it should "allow the address to be extracted (listLens)" in {
  //    setup { exampleDomain =>
  //      import exampleDomain._
  //
  //      val thePayload = payload(json)
  //
  //      val person: Person = root.get(thePayload)
  //      val addresses: List[Address] = person_addresses.get(person)
  //      addresses.map(address_line1.get) shouldBe List("No fixed abode", "A second address")
  //      //
  //    }
  //  }
  //  it should "allow the address to be manipulated (listLens)" in {
  //    setup { exampleDomain =>
  //      import exampleDomain._
  //
  //      val thePayload = payload(json)
  //      val person: Person = root.get(thePayload)
  //      val addresses: List[Address] = person_address_list.get(person)
  //
  //
  //      addresses.map(address_line1.get) shouldBe List("No fixed abode", "A second address")
  //
  //
  //      val addresses2: List[Address] = addresses.map(address_line1.map(_ + "changed"))
  //      addresses2.map(address_line1.get) shouldBe List("No fixed abodechanged", "A second addresschanged")
  //
  //      val person2 = person_address_list.set(person, addresses2)
  //      val addresses3: List[Address] = person_address_list.get(person2)
  //      addresses3.map(address_line1.get) shouldBe List("No fixed abodechanged", "A second addresschanged")
  //    }
  //  }
}
