/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddengine
import one.xingyi.cddscenario.{No, ScBuilder, Scenario, Yes}
import one.xingyi.core.UtilsSpec

import scala.language.reflectiveCalls
class UseCaseBuilderSpec extends UtilsSpec {

  behavior of "Use case builder"

  it should "allow scenarios to be 'added' to it using the apply method" in {
    var list = List[Scenario[Int, String]]()
    val uc = new UseCase1[Int, String]("some usecase") {
      val s1: ScBuilder[Int, String, Yes, No, No, Yes] = scenario(1) produces "1" because { case x => x.toString }
      val s2 = scenario(2) produces "2"
      //      list = List(s1.asScenario, s2.scenario)
    }
    //    uc.s1.data.isDefinedAt.toString shouldBe "(UseCaseBuilderSpec.scala:13)"
    //    uc.s2.data.isDefinedAt.toString shouldBe "(UseCaseBuilderSpec.scala:14)"
    //    uc.allScenarios shouldBe List(uc.s1.scenario, uc.s2.scenario)
  }
}
