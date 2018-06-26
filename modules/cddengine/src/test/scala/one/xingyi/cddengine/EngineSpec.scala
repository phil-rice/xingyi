/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddengine
import one.xingyi.cddscenario.{SingleScenarioLogic, WhenResultScenarioLogic}
import one.xingyi.core.UtilsSpec

trait EngineFixture extends DecisionTreeFixture {
  val containsA = { s: String => s contains "a" }
  val containsB = { s: String => s contains "b" }

  val usecase1 = new UseCase1[String, String]("use case about A") {
    scenario("a") produces "A" when containsA
     scenario("aw") produces "A"
  }
  val usecase2 = new UseCase1[String, String]("use case about B") {
    val b = scenario("b") produces "B"
    val bw = scenario("bw") produces "B"
  }
  val e = Engine(usecase1 or usecase2)


}
class EngineSpec extends UtilsSpec with EngineFixture {

  behavior of "Engine smoke tests"


  it should "not smoke" in {


    val DecisionTree(DecisionNode(dlLogic: SingleScenarioLogic[String, String],
    ConclusionNode(List(sb, sbw), falseLogic: SingleScenarioLogic[String, String]),
    ConclusionNode(List(sa, saw), trueLogic: SingleScenarioLogic[String, String])), List()) = e.tools.decisionTree
    sb.situation shouldBe "b"
    sbw.situation shouldBe "bw"
    sa.situation shouldBe "a"
    saw.situation shouldBe "aw"
    dlLogic.asInstanceOf[WhenResultScenarioLogic[String, String]].when shouldBe containsA
    trueLogic shouldBe dlLogic

    e("a") shouldBe "A"
    e("b") shouldBe "B"
    e("c") shouldBe "B"
  }
  it should "not smoke with deeper tests" in {

    val e = Engine(new UseCase1[String, String]("engine") {
      scenario("a") produces "A" when containsA
      scenario("aw") produces "A"
      scenario("c") produces "C"
      scenario("cw") produces "C"
      scenario("b") produces "B" when containsB
      scenario("bw") produces "B"

    })

    val DecisionTree(
    DecisionNode(dlLogic: SingleScenarioLogic[String, String],
    DecisionNode(ifcLogic: SingleScenarioLogic[String, String],
    ConclusionNode(List(sc, scw), cLogic: SingleScenarioLogic[String, String]),
    ConclusionNode(List(sb, sbw), bLogic: SingleScenarioLogic[String, String])),
    ConclusionNode(List(sa, saw), aLogic: SingleScenarioLogic[String, String])), List()) = e.tools.decisionTree

    sb.situation shouldBe "b"
    sbw.situation shouldBe "bw"
    sa.situation shouldBe "a"
    saw.situation shouldBe "aw"
    sc.situation shouldBe "c"
    scw.situation shouldBe "cw"
    dlLogic.asInstanceOf[WhenResultScenarioLogic[String, String]].when shouldBe containsA


    e("a") shouldBe "A"
    e("b") shouldBe "B"
    e("c") shouldBe "C"
    e("d") shouldBe "C"
  }

}
