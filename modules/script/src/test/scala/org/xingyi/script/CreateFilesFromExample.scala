package org.xingyi.script

import java.io.File

import one.xingyi.core.UtilsSpec
import one.xingyi.core.strings.Files

class CreateFilesFromExample extends UtilsSpec {

  val scalaFile = new File("modules/scriptExample/src/test/scala/org/xingyi/scriptExample/createdCode/Example.scala").getAbsoluteFile
  val javascriptFile = new File("modules/scriptExample/src/test/resources/example.js").getAbsoluteFile
  behavior of "Create The Example Domain Classes"

  val isCorrectDirectory = scalaFile.getPath.replace('/', '\\').endsWith("xingyi\\modules\\scriptExample\\src\\test\\scala\\org\\xingyi\\scriptExample\\createdCode\\Example.scala")
  it should "be talking to the correct directory" in {
    checkCorrectDirectory
  }

  def checkCorrectDirectory = {
    withClue(s"directory is $scalaFile")(isCorrectDirectory shouldBe true)
  }

  it should "make the javascript" in {
    checkCorrectDirectory

    val codeMaker = implicitly[HasLensCodeMaker[Javascript]]
    val javascript = codeMaker.apply(new ExampleDomain)
    Files.printToFile(javascriptFile)(pw => pw.print(javascript))
  }

  it should "make the scala" in {
    val scala = ScalaTrait.makeFile("org.xingyi.scriptExample.createdCode",new ExampleDomain)
    Files.printToFile(scalaFile)(pw => pw.print(scala))

  }

}
