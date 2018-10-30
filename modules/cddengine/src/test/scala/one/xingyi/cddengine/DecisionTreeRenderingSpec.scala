/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddengine
import one.xingyi.core.UtilsSpec
import one.xingyi.core.json.JsonLanguage.toJsonString
import one.xingyi.core.json.{JsonList, JsonObject, JsonWriterLangauge}

class DecisionTreeRenderingSpec extends UtilsSpec with DecisionTreeFixture with JsonWriterLangauge {

  behavior of "SimpleDecisionTreeRendering"

  val sPassportRendered = JsonObject("situation" -> "woman", "url" -> "scenario_0.html", "defined" -> sNoPassport.data.definedInSourceCodeAt.toString)
  val sManWIthGunAndPassportRendered = JsonObject("situation" -> "man with gun and passport", "url" -> "scenario_1.html", "defined" -> sgun.definedInSourceCodeAt.toString)
  val sManWithGunNoPasswportRendered = JsonObject("situation" -> "man with gun", "url" -> "scenario_2.html", "defined" -> sgunNoPassport.definedInSourceCodeAt.toString)
  val concGunNoPasswordRendered = JsonObject("conclusionNode" ->
                                             JsonObject("scenarios" -> JsonList(List(sManWIthGunAndPassportRendered, sManWithGunNoPasswportRendered))), "defined" -> sgun.definedInSourceCodeAt.toString)

  val simple = DecisionTreeRendering.simple[String, String]

  it should "render a scenario" in {
    simple.scenario(sNoPassport) shouldBe sPassportRendered
  }

  it should "render a conclusion node" in {
    simple.node(concGunGunNoPassport) shouldBe concGunNoPasswordRendered
  }

  it should "render a decision node" in {
    simple.node(dNormalGun) shouldBe JsonObject(
      "decisionNode" ->
      JsonObject("condition" -> sgun.logic.ifString, "ifTrue" -> simple.conclusionNode(concGun), "ifFalse" -> simple.conclusionNode(concNormal)),
      "defined" -> sgun.definedInSourceCodeAt.toString)
  }

  it should ""
}
