package one.xingyi.kwikServer

import java.io
import java.io.File

import one.xingyi.core.strings.Files
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class PomBundleToFilesTest extends FlatSpec with Matchers with PomBundleFixture {
  behavior of PomBundleToFiles.getClass.getSimpleName

  it should "create the files with contents defined by PomBundle" in {
    val dir: io.File = PomBundleToFiles.toTempFile(pomBundle)
    val pom1 = Source.fromFile(new File(dir, "pom.xml")).mkString
    val pom2 = Source.fromFile(new File(dir, "sample1/pom.xml")).mkString
    pom1.trim shouldBe sampleParentPom
    pom2.trim shouldBe samplePom
  }


}
