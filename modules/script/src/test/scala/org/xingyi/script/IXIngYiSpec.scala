package org.xingyi.script
import one.xingyi.core.UtilsSpec
import one.xingyi.core.json.{JsonObject, JsonValue, JsonWriterLangauge}

import scala.io.Source

class IXIngYiSpec extends UtilsSpec with JsonWriterLangauge {

  behavior of "IXingYi"

  it should "allow the person's name to be extracted" in {
    val xingyi = implicitly[IXingYiLoader[Object]].apply("demo.js")
    val json = Source.fromInputStream(getClass.getResourceAsStream("/sample.json")).mkString
//    println(json)
    val j = xingyi.parse(json)
//    (xingyi.rootLens ).get(j) shouldBe "Phil Rice"
    val namesLens = xingyi.stringLens("person_name")
    namesLens.get(j) shouldBe "Phil Rice"
    val j1 = namesLens.set(j, "New Name")
    namesLens.get(j1) shouldBe "New Name"
    println
    println(xingyi.render("json", j))
    println
    println(xingyi.render("json", j1))
    namesLens.get(j1) shouldBe "New Name"



  }
}
