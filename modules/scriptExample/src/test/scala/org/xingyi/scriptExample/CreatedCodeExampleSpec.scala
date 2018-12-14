package org.xingyi.scriptExample

import one.xingyi.core.UtilsSpec
import org.xingyi.script.{IXingYi, IXingYiLoader}
import org.xingyi.scriptExample.createdCode._

import scala.collection.immutable
import scala.io.Source

class CreatedCodeExampleSpec extends UtilsSpec {

  val json = Source.fromInputStream(getClass.getResourceAsStream("/sample.json")).mkString
  behavior of "Example"

  def setup(fn: (org.xingyi.scriptExample.createdCode.ExampleDomain) => Unit): Unit = {
    val javascript = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("example.js")).mkString
    implicit val xingyi = implicitly[IXingYiLoader].apply(javascript)
    fn(new ExampleDomain)
  }


  it should "allow the person's name (lens and stringLens) to be extracted" in {
    setup { exampleDomain =>
      import exampleDomain._
      val thePayload = payload(json)

      val namesLens = root andThen person_name

      val payload1 = namesLens.set(thePayload, "New Name")

      namesLens.get(thePayload) shouldBe "Phil Rice"
      val payload2 = namesLens.set(payload1, "Newer Name")
      namesLens.get(payload1) shouldBe "New Name"
      namesLens.get(payload2) shouldBe "Newer Name"

    }
  }
  it should "allow the address to be extracted (listLens)" in {
    setup { exampleDomain =>
      import exampleDomain._
      val thePayload = payload(json)

      val person: Person = root.get(thePayload)
      val addresses: List[Address] = person_address_list.get(person)
      addresses.map(address_line1.get) shouldBe List("No fixed abode", "A second address")
      //
    }
  }
  it should "allow the address to be manipulated (listLens)" in {
    setup { exampleDomain =>
      import exampleDomain._
      val thePayload = payload(json)

      val person: Person = root.get(thePayload)
      val addresses: List[Address] = person_address_list.get(person)
      val addresses2: List[Address] = addresses.map(address_line1.map(_ + "changed"))

      addresses.map(address_line1.get) shouldBe List("No fixed abode", "A second address")
      addresses2.map(address_line1.get) shouldBe List("No fixed abodechanged", "A second addresschanged")

      val person2 = person_address_list.set(person, addresses2)
      val addresses3: List[Address] = person_address_list.get(person2)
      addresses3.map(address_line1.get) shouldBe List("No fixed abodechanged", "A second addresschanged")
    }
  }
}