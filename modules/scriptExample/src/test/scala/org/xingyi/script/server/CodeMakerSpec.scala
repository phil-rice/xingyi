package org.xingyi.script.server

import org.scalatest.{FlatSpec, Matchers}

trait Root

trait Person

trait Address

class ExampleHasLens {
  val rootL = LensDefn[Root, Person]("root", List("payload", "_embedded"))
  val personNameL = LensDefn[Person, String]("person_name", List("name"))
  val personAddressL = LensDefn[Person, Address]("person_address", List("address"))
  val addressNameL = LensDefn[Address, String]("address_name", List("name"))
}

class CodeMakerSpec extends FlatSpec with Matchers {

  behavior of "Lens code maker"

  it should "make a string with a function for each lens" in {
    println
    println
    println(implicitly[HasLensCodeMaker[Javascript]].apply(new ExampleHasLens))
    println
    println
    println(implicitly[HasLensCodeMaker[ScalaDomain]].apply(new ExampleHasLens))

    println(implicitly[HasLensCodeMaker[ScalaTrait]].apply(new ExampleHasLens))

  }

}
