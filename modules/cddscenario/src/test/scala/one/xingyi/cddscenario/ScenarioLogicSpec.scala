/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddscenario

import one.xingyi.core.functions.SemiGroup
import one.xingyi.core.{SemiGroupSpec, UtilsSpec}

import scala.reflect.ClassTag

class ScenarioLogicSpec extends UtilsSpec {

  behavior of "ScenarioLogic"

  it should "have an empty method" in {
    val empty: NoScenarioLogic[Int, Int] = ScenarioLogic.empty[Int, Int]
    empty.ifString shouldBe "<empty>"
    empty.definedInSourceCodeAt.toString shouldBe "(ScenarioLogicSpec.scala:14)"
  }

}


class SemigroupForScenarioLogicSpec extends SemiGroupSpec[ScenarioLogic[String, Int]] {
  override def classTag: ClassTag[ScenarioLogic[String, Int]] = implicitly[ClassTag[ScenarioLogic[String, Int]]]
  override def semiGroup: SemiGroup[ScenarioLogic[String, Int]] =implicitly[SemiGroup[ScenarioLogic[String,Int]]]
  override val one: SingleScenarioLogic[String, Int] = mock[SingleScenarioLogic[String,Int]]
  override val two: SingleScenarioLogic[String, Int] = mock[SingleScenarioLogic[String,Int]]
  override val three: ScenarioLogic[String, Int] = CompositeScenarioLogic(Seq(one, two))
}