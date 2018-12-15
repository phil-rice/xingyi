/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddscenario

import one.xingyi.core.functions.SemiGroup
import one.xingyi.core.reflection.{IsDefinedInSourceCodeAt, SingleDefinedInSourceCodeAt}
import one.xingyi.core.{SemiGroupSpec, UtilsSpec}

import scala.reflect.ClassTag

class ScenarioLogicSpec extends UtilsSpec {

  behavior of "ScenarioLogic"

  it should "have an empty method" in {
    val empty: NoScenarioLogic[Int, Int] = ScenarioLogic.empty[Int, Int]
    empty.ifString shouldBe "<empty>"
    empty.definedInSourceCodeAt.toString shouldBe "(ScenarioLogicSpec.scala:15)"
  }


}


class SemigroupForScenarioLogicSpec extends SemiGroupSpec[ScenarioLogic[String, Int]] {
  override def classTag: ClassTag[ScenarioLogic[String, Int]] = implicitly[ClassTag[ScenarioLogic[String, Int]]]
  override def semiGroup: SemiGroup[ScenarioLogic[String, Int]] = implicitly[SemiGroup[ScenarioLogic[String, Int]]]
  override val one: SingleScenarioLogic[String, Int] = mock[SingleScenarioLogic[String, Int]]
  override val two: SingleScenarioLogic[String, Int] = mock[SingleScenarioLogic[String, Int]]
  override val three: ScenarioLogic[String, Int] = CompositeScenarioLogic(Seq(one, two))
}

abstract class SingleScenarioLogicSpec[SL <: SingleScenarioLogic[String, Int] : ClassTag] extends UtilsSpec {
  import ScenarioLogic._
  def scenarioLogic: SL

  val name: String = implicitly[ClassTag[SL]].runtimeClass.getSimpleName
  behavior of name

  it should "have an implicitly 'isDefinedInSourceCodeAt' which return the 'isDefinedInSourceCode At" in {
    val sl = scenarioLogic
    implicitly[IsDefinedInSourceCodeAt[ScenarioLogic[String, Int]]].apply(sl) shouldBe sl.definedInSourceCodeAt
  }
  it should "have a nice to string" in {
    val sl = scenarioLogic
    sl.toString shouldBe s"$name@(${sl.definedInSourceCodeAt})"
  }

}

class NoScenarioLogicSpec extends SingleScenarioLogicSpec[NoScenarioLogic[String, Int]] {
  override lazy val scenarioLogic: NoScenarioLogic[String, Int] = NoScenarioLogic(mock[SingleDefinedInSourceCodeAt], "someIfString")

  it should "not have a condition" in {
    scenarioLogic.hasCondition shouldBe false
  }

  it should "blow up if the fn is called" in {
    intercept[RuntimeException](scenarioLogic.fn("someString")).getMessage shouldBe "someString (of class java.lang.String)"
  }
}

class ResultScenarioLogicSpec extends SingleScenarioLogicSpec[ResultScenarioLogic[String, Int]] {
  override lazy val scenarioLogic: ResultScenarioLogic[String, Int] = ResultScenarioLogic(123, mock[SingleDefinedInSourceCodeAt], "someIfString")

  it should "not have a condition" in {
    scenarioLogic.hasCondition shouldBe false
  }

  it should "return the result if the fn is called" in {
    scenarioLogic.fn("someString") shouldBe 123
  }
}
class BecauseScenarioLogicSpec extends SingleScenarioLogicSpec[BecauseScenarioLogic[String, Int]] {
  override lazy val scenarioLogic: BecauseScenarioLogic[String, Int] = BecauseScenarioLogic({ case s: String if s.toInt > 2 => s.toInt }, mock[SingleDefinedInSourceCodeAt], "someIfString")

  it should "have a condition" in {
    scenarioLogic.hasCondition shouldBe true
  }

  it should "use the partial function passed into it" in {
    scenarioLogic.fn.isDefinedAt("0") shouldBe false
    scenarioLogic.fn.isDefinedAt("3") shouldBe true
    scenarioLogic.fn("3") shouldBe 3
  }
}

class WhenResultScenarioLogicSpec extends SingleScenarioLogicSpec[WhenResultScenarioLogic[String, Int]] {
  override lazy val scenarioLogic: WhenResultScenarioLogic[String, Int] = WhenResultScenarioLogic(x => x.toInt > 2, 123, mock[SingleDefinedInSourceCodeAt], "someIfString")

  it should "have a condition" in {
    scenarioLogic.hasCondition shouldBe true
  }

  it should "use the when as the 'if defined' " in {
    scenarioLogic.fn.isDefinedAt("0") shouldBe false
    scenarioLogic.fn.isDefinedAt("3") shouldBe true
  }
  it should "use the result" in {
    //    scenarioLogic.fn("0") shouldBe 123
    scenarioLogic.fn("3") shouldBe 123
  }
}

class WhenCodeScenarioLogicSpec extends SingleScenarioLogicSpec[WhenCodeScenarioLogic[String, Int]] {
  override lazy val scenarioLogic: WhenCodeScenarioLogic[String, Int] = WhenCodeScenarioLogic(x => x.toInt > 2, x => x.toInt * 2, mock[SingleDefinedInSourceCodeAt], "someIfString")

  it should "have a condition" in {
    scenarioLogic.hasCondition shouldBe true
  }

  it should "use the when as the 'if defined' " in {
    scenarioLogic.fn.isDefinedAt("0") shouldBe false
    scenarioLogic.fn.isDefinedAt("3") shouldBe true
  }
  it should "use the code" in {
    scenarioLogic.fn("3") shouldBe 6
  }
}

import org.mockito.Mockito._
class CompositeScenarioLogicSpec extends UtilsSpec {
  behavior of "CompositeScenarioLogic"

  def setup[X](hc1: Boolean = true, hc2: Boolean = true)(fn: (CompositeScenarioLogic[String, Int], SingleScenarioLogic[String, Int], SingleScenarioLogic[String, Int]) => X) = {
    val s1 = mock[SingleScenarioLogic[String, Int]]
    val s2 = mock[SingleScenarioLogic[String, Int]]
    when(s1.hasCondition) thenReturn (hc1)
    when(s2.hasCondition) thenReturn (hc2)
    when(s1.ifString) thenReturn ("s1IfString")
    when(s2.ifString) thenReturn ("s2IfString")
    fn(new CompositeScenarioLogic(Seq(s1, s2)), s1, s2)
  }

  it should "have a 'hasCondition' if one of the sceanarios does" in {
    setup(false, false) { (comp, s1, s2) => comp }.hasCondition shouldBe false
    setup(false, true) { (comp, s1, s2) => comp }.hasCondition shouldBe true
    setup(true, false) { (comp, s1, s2) => comp }.hasCondition shouldBe true
    setup(true, true) { (comp, s1, s2) => comp }.hasCondition shouldBe true
  }

  it should "an ifstring that is the composition of the scenarios" in {
    setup() { (comp, s1, s2) => comp }.ifString shouldBe "s1IfString or s2IfString"
  }

  it should "a toString that is the composition of the scenarios" in {
    setup() { (comp, s1, s2) =>
      comp.toString() shouldBe s"CompLogic($s1,$s2)"
    }
  }
  it should "a fn that is the composition of the scenarios" in {
    setup() { (comp, s1, s2) =>
      val pf1 = mock[PartialFunction[String,Int]]
      val pf2 = mock[PartialFunction[String,Int]]
      val res = mock[PartialFunction[String, Int]]
      when (s1.fn) thenReturn pf1
      when (s2.fn) thenReturn pf2
      when(pf1.orElse(pf2)) thenReturn res

      comp.fn shouldBe res
    }
  }
}
