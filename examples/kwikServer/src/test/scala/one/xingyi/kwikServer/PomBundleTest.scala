package one.xingyi.kwikServer

import one.xingyi.core.crypto.Digestor
import one.xingyi.core.strings.Files
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

trait PomBundleFixture{
  def fromResource(s: String)=Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(s)).getLines().mkString("\n")
  val sampleParentPom = fromResource("sampleParentPom.xml")
  val samplePom = fromResource("samplePom.xml")
  val pomBundleString =
    s"""repositories 2
      |repoUrl1
      |repoUrl2
      |environment 2
      |env1 one
      |evn2 two
      |system 2
      |prop1 one
      |prop2 two
      |pom.xml 34
      |$sampleParentPom
      |sample1/pom.xml 22
      |$samplePom
      |""".stripMargin
  val pomBundle = PomBundle(
    List("repoUrl1", "repoUrl2"),
    List(
      Variable("env1", "one"),
      Variable("evn2", "two")),
    List(
      Variable("prop1", "one"),
      Variable("prop2", "two")),
    List(
      PomData("pom.xml", sampleParentPom),
      PomData("sample1/pom.xml", samplePom)
    ),
    implicitly[Digestor].apply(pomBundleString)
  )

}
class PomBundleTest extends FlatSpec with Matchers with KwikFixture with PomBundleFixture {

  behavior of PomBundle.getClass.getSimpleName

  it should "parse from a string" in {
    PomBundle.parse(pomBundleString) shouldBe pomBundle
  }


}
