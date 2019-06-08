/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddscenario

import one.xingyi.core.UtilsSpec

import scala.language.higherKinds


class EngineBuilderLanguage1Spec extends UtilsSpec with EngineBuilderLanguage1 {

  behavior of "EngineBuilderLanguage1"

  it should "have a is defined at" in {
    val (x, scenarios) = new RememberingScenarioAggregator2[Int, String].withAggreator { implicit a =>
      val x = scenario(2) produces "2"
      x.data.definedInSourceCodeAt.toString shouldBe "(EngineBuilderLanguage1Spec.scala:15)"
      x
    }
    scenarios shouldBe List(x.scenario)
  }


  it should "allow scenarios to be created" in {
    val (result, scenarios) = new RememberingScenarioAggregator2[Int, String].withAggreator { implicit a =>
      val x1 = scenario(1) produces "1"
      val x2 = scenario(2) produces "2"
      val y = scenario(2) produces "2" when (_ < 10) title "this is case 2" comment "not very interesting"
      val z = scenario(3) produces "3" because { case x if x < 10 => x.toString }
      val wc = scenario(1) produces "2" when (_ < 10) code (x => (x + 1).toString) title "this is case wc" reference (InternetDocument("name", "ref"))

      List(x1, x2, y, z, wc)
    }
    scenarios shouldBe result.map(_.scenario)
    val List(x1, x2, y, z, wc) = scenarios

    z.logic.fn.isDefinedAt(-5) shouldBe true
    z.logic.fn.isDefinedAt(9) shouldBe true
    z.logic.fn.isDefinedAt(10) shouldBe false
    z.logic.fn.isDefinedAt(11) shouldBe false
    z.logic.fn(3) shouldBe "3"

    wc.logic.fn(1) shouldBe "2"
    wc.logic.fn(2) shouldBe "3"

    scenarios.map(_.data.definedInSourceCodeAt.toString) shouldBe List(
      "(EngineBuilderLanguage1Spec.scala:25)",
      "(EngineBuilderLanguage1Spec.scala:26)",
      "(EngineBuilderLanguage1Spec.scala:27)",
      "(EngineBuilderLanguage1Spec.scala:28)",
      "(EngineBuilderLanguage1Spec.scala:29)"
    )
  }


}
