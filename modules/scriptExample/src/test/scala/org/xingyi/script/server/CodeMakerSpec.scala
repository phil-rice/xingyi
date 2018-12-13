package org.xingyi.script.server

import org.scalatest.{FlatSpec, Matchers}
import org.xingyi.script.{IXingYi, IXingYiLoader, Payload}

import scala.io.Source

class CodeMakerSpec extends FlatSpec with Matchers {

  behavior of "Java script code maker"
  val exampleDomain = new ExampleDomain

  val json = Source.fromInputStream(getClass.getResourceAsStream("/sample.json")).mkString
  def setup(fn: IXingYi => Unit): Unit = {
    val codeMaker = implicitly[HasLensCodeMaker[Javascript]]
    val javascript = codeMaker.apply(exampleDomain)
    val xingyi = implicitly[IXingYiLoader].apply(javascript)
    fn(xingyi)
  }

  it should "parse then render" in {
    setup { xingyi =>
      println("json: " + xingyi.render("pretty", xingyi.parse(json)))
    }

  }

  it should "by able to get the person" in {
    setup { xingyi =>
      println("json: " + xingyi.objectLens[Payload, Payload]("root").get(xingyi.parse(json)))
    }
  }

  //This is an integration test that checks the javascript loaded along with IXingYi
  it should "allow the person's name to be extracted" in {
    setup { xingyi =>

      val j = xingyi.parse(json)
      println(json)

      val namesLens = xingyi.objectLens[Payload, Payload]("root") andThen xingyi.stringLens[Payload]("person_name")
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

  it should "make a scala class" in {
    val file = List("package org.xingyi.testing", implicitly[HasLensCodeMaker[ScalaDomain]].apply(exampleDomain), implicitly[HasLensCodeMaker[ScalaTrait]].apply(exampleDomain)).mkString("\n")
    println(file)
  }

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
