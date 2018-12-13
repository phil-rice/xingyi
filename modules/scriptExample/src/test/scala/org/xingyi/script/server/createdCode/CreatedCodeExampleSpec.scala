package org.xingyi.script.server.createdCode
import one.xingyi.core.UtilsSpec
import org.xingyi.script.{IXingYiLoader, Payload}
import org.xingyi.script.server.{HasLensCodeMaker, Javascript}

import scala.io.Source

class CreatedCodeExampleSpec extends UtilsSpec {

  behavior of "Example"

  it should "allow the person's name to be extracted" in {
    //    HasLensCodeMaker.maker[Javascript]
    val codeMaker = implicitly[HasLensCodeMaker[Javascript]]
    val javascript = codeMaker.apply(new org.xingyi.script.server.ExampleDomain)
    val xingyi = implicitly[IXingYiLoader].apply(javascript)

    val json = Source.fromInputStream(getClass.getResourceAsStream("/sample.json")).mkString
    val j = xingyi.parse(json)

    val namesLens = xingyi.objectLens[Payload, Payload]("root") andThen xingyi.stringLens[Payload]("person_name")
    namesLens.get(j) shouldBe "Phil Rice"
    val j1 = namesLens.set(j, "New Name")
    val j2 = namesLens.set(j1, "Newer Name")

    namesLens.get(j1) shouldBe "New Name"
    namesLens.get(j2) shouldBe "Newer Name"

    namesLens.get(j1) shouldBe "New Name"
    namesLens.get(j2) shouldBe "Newer Name"
  }
}
