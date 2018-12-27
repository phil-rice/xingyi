/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.scriptBackend3

import java.io.File

import one.xingyi.core.UtilsSpec
import one.xingyi.core.script.{DomainDefn, HasLensCodeMaker, Javascript, ToScalaCode}
import one.xingyi.core.strings.Files

class CreateFilesFromExample3 extends UtilsSpec {

  val scalaFile = new File("modules/scriptWebsite/src/main/scala/one/xingyi/scriptWebsite/createdCode3/Example3.scala").getAbsoluteFile
  val javascriptFile = new File("modules/scriptWebsite/src/main/resources/example3.js").getAbsoluteFile
  behavior of "Create The Example Domain Classes"

  val isCorrectDirectory = scalaFile.getPath.replace('/', '\\').endsWith("xingyi\\modules\\scriptWebsite\\src\\main\\scala\\one\\xingyi\\scriptWebsite\\createdCode3\\Example3.scala")
  it should "be talking to the correct directory" in {
    checkCorrectDirectory
  }

  def checkCorrectDirectory = {
    withClue(s"directory is $scalaFile")(isCorrectDirectory shouldBe true)
  }

  it should "make the javascript" in {
    checkCorrectDirectory

    val codeMaker = implicitly[HasLensCodeMaker[Javascript]]
    val javascript = codeMaker.apply(new Model3Defn)
    Files.printToFile(javascriptFile)(pw => pw.print(javascript))
  }

  it should "make the scala" ignore {
    ToScalaCode.makeScalaCode[Person]
    val codeMaker = implicitly[ToScalaCode[DomainDefn[Person]]]
    val scala = codeMaker(new Model3Defn)
    Files.printToFile(scalaFile)(pw => pw.print(scala))

  }

}
