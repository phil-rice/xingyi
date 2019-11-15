package one.xingyi.kwikServer

import org.scalatest.{FlatSpec, Matchers}

class PomBundleTest extends FlatSpec with Matchers with KwikFixture {

  behavior of PomBundle.getClass.getSimpleName
  val pomBundleString =
    """environment 2
      |env1 one
      |evn2 two
      |system 2
      |prop1 one
      |prop2 two
      |pom1loc 3
      |pom11
      |pom12
      |pom13
      |pom2loc 1
      |pom21
      |""".stripMargin
  val expected = PomBundle(
    List(
      Variable("env1", "one"),
      Variable("evn2", "two")),
    List(
      Variable("prop1", "one"),
      Variable("prop2", "two")),
    List(
      PomData("pom1loc", List("pom11", "pom12", "pom13").mkString("\n")),
      PomData("pom2loc", "pom21")
    ),
    "somehash"
  )

  it should "parse from a string" in {
    PomBundle.parse(pomBundleString) shouldBe expected
  }


}
