package org.xingyi.script.server.createdCode
import one.xingyi.core.UtilsSpec
import org.xingyi.script.{IXingYi, IXingYiLoader, Payload}
import org.xingyi.script.server.{HasLensCodeMaker, Javascript}

import scala.io.Source

class CreatedCodeExampleSpec extends UtilsSpec {

  val json = Source.fromInputStream(getClass.getResourceAsStream("/sample.json")).mkString
  behavior of "Example"
  def setup(fn: (IXingYi, ExampleDomain) => Unit): Unit = {
    val codeMaker = implicitly[HasLensCodeMaker[Javascript]]
    val javascript = codeMaker.apply(new org.xingyi.script.server.ExampleDomain)
    implicit val xingyi = implicitly[IXingYiLoader].apply(javascript)
    fn(xingyi, new ExampleDomain)
  }


  it should "allow the person's name to be extracted" in {
    setup { (xingyi, exampleDomain: ExampleDomain) => //    HasLensCodeMaker.maker[Javascript]
      import exampleDomain._
      val thePayload = payload(json)
      val namesLens = root andThen person_name

      val person: Person = root.get(thePayload)
      val address: Address = person_address.get(person)

      //      val namesLens = xingyi.objectLens[Payload, Payload]("root") andThen xingyi.stringLens[Payload]("person_name")
      namesLens.get(thePayload) shouldBe "Phil Rice"
      val payload1 = namesLens.set(thePayload, "New Name")
      val payload2 = namesLens.set(payload1, "Newer Name")
      namesLens.get(payload1) shouldBe "New Name"
      namesLens.get(payload2) shouldBe "Newer Name"

      (root andThen person_address andThen address_line1).get(thePayload) shouldBe "No fixed abode"
      address_line1.get(address) shouldBe "No fixed abode"
    }
  }
}
