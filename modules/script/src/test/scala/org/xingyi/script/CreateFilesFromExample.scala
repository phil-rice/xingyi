/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
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
