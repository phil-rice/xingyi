/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddscenario

import one.xingyi.cddscenario.EngineComponentData._
import one.xingyi.core.misc.IdMaker
import one.xingyi.core.optics.Lens

import scala.annotation.implicitNotFound
import scala.language.experimental.macros
import scala.language.higherKinds


sealed trait YesNo
trait Yes extends YesNo
trait No extends YesNo

//just to make equals work. Will probably remove when refactor scenario logic
//case class BecauseDefinedAt[P, R](becauseFn: PartialFunction[P, R]) extends (P => Boolean) {
//  override def apply(p: P): Boolean = becauseFn.isDefinedAt(p)
//}
class ScBuilder[P, R, HasResult, HasWhen, HasCode, HasBecause](protected[cddscenario] val id: Int,
                                                               protected[cddscenario] val situation: P,
                                                               protected[cddscenario] val data: EngineComponentData,
                                                               protected[cddscenario] val optResult: Option[R] = None,
                                                               protected[cddscenario] val optWhen: Option[P => Boolean] = None,
                                                               protected[cddscenario] val optCode: Option[P => R] = None,
                                                               protected[cddscenario] val optBecause: Option[PartialFunction[P, R]] = None,
                                                               protected[cddscenario] val ifString: String,
                                                               protected[cddscenario] val thenString: String)(implicit protected[cddscenario] val scenarioAggregator2: ScenarioAggregator2[P, R]) {
  scenarioAggregator2(this)
  protected[cddscenario] def withData(data: EngineComponentData) = new ScBuilder[P, R, HasResult, HasWhen, HasCode, HasBecause](id, situation, data, optResult, optWhen, optCode, optBecause, ifString, thenString)

  //TODO revisit this and sort out scenario logic. Also need the if string and perhaps the then string
  protected[cddscenario] def scenarioLogic: SingleScenarioLogic[P, R] = (optWhen, optCode, optBecause, optResult) match {
    case (Some(w), Some(c), None, _) => WhenCodeScenarioLogic(w, c, data.definedInSourceCodeAt, ifString)
    case (Some(w), None, None, Some(r)) => WhenResultScenarioLogic(w, r, data.definedInSourceCodeAt, ifString)
    case (None, None, Some(b), _) => BecauseScenarioLogic(b, data.definedInSourceCodeAt, ifString)
    case (None, None, None, Some(r)) => ResultScenarioLogic(r, data.definedInSourceCodeAt, ifString)
    case x => throw new RuntimeException(s"Unexpected pattern of whens and becauses and stuff $x")
  }
  def scenario = Scenario[P, R](situation, optResult, scenarioLogic, List(), data)
  protected[cddscenario] def withResultPrim(r: R) = new ScBuilder[P, R, Yes, HasWhen, No, No](id, situation, data, Some(r), optWhen, optCode, optBecause, ifString, thenString)
//  protected[cddscenario] def withBecausePrim(because: PartialFunction[P, R], ifString: String, thenString: String) = new ScBuilder[P, R, HasResult, No, HasCode, Yes](id, situation, data, optResult, optWhen, optCode, Some(because), ifString, thenString)
}


object ScBuilder {
  implicit def toData[P, R, HasResult, HasWhen, HasCode, HasBecause](implicit scenarioAggregator2: ScenarioAggregator2[P, R]): Lens[ScBuilder[P, R, HasResult, HasWhen, HasCode, HasBecause], EngineComponentData] =
    Lens(_.data, (w, d) => w.withData(data = d))
}

@implicitNotFound("""If you are making the scenario using a use case you will have one of these. If you NEED to have no aggregator and know what you are doing, you can import the NullScenarioAggregator""")
trait ScenarioAggregator2[P, R] extends (ScBuilder[P, R, _, _, _, _] => Unit)

object NullScenarioAggregator2 {
  implicit def nullAggregator[P, R]: ScenarioAggregator2[P, R] = scenario => {}
}

class RememberingScenarioAggregator2[P, R] extends ScenarioAggregator2[P, R] {
  private var list = List[ScBuilder[P, R, _, _, _, _]]()
  private val lock = new Object()
  def withAggreator[X](fn: ScenarioAggregator2[P, R] => X): (X, List[Scenario[P, R]]) = (fn(this), scenarios)

  def scenarios = list.map(_.scenario)
  override def apply(comp: ScBuilder[P, R, _, _, _, _]): Unit = list = list.filterNot(_.id == comp.id) :+ comp
}

trait HasScBuilder[P, R, HasResult, HasWhen, HasCode, HasBecause] {
  def builder: ScBuilder[P, R, HasResult, HasWhen, HasCode, HasBecause]
}

object EngineBuilderLanguage {
  def withBecausePrim[P, R, HasResult](builder: ScBuilder[P, R, HasResult, No, No, No], because: PartialFunction[P, R], ifStringFromMacro: String, thenStringFromMacro: String) = {
    import builder._
    new ScBuilder[P, R, HasResult, No, No, Yes](id, situation, data, optResult, optWhen, optCode, Some(because), ifStringFromMacro, thenStringFromMacro)
  }
  def withWhenPrim[P, R, HasResult, HasCode](builder: ScBuilder[P, R, HasResult, No, HasCode, No], when: P => Boolean, ifStringFromMacro: String): ScBuilder[P, R, HasResult, Yes, HasCode, No] = {
    import builder._
    new ScBuilder[P, R, HasResult, Yes, HasCode, No](id, situation, data, optResult, Some(when), optCode, optBecause, ifStringFromMacro, thenString)
  }
  def withCodePrim[P, R, HasResult, HasWhen](builder: ScBuilder[P, R, HasResult, HasWhen, No, No], code: P => R, codeStringFromMacro: String): ScBuilder[P, R, HasResult, HasWhen, Yes, No] = {
    import builder._
    new ScBuilder[P, R, HasResult, HasWhen, Yes, No](id, situation, data, optResult, optWhen, Some(code), optBecause, ifString, codeStringFromMacro)
  }

}
trait EngineBuilderLanguage extends IdMaker {
  implicit class LensToEngineDataOps[T](t: T)(implicit toDataL: Lens[T, EngineComponentData]) {
    def comment(comment: String) = toDataL andThen toCommentL set(t, Some(comment))
    def title(title: String) = toDataL andThen toTitleL set(t, Some(title))
    def reference(doc: Document) = toDataL andThen toReferencesL transform(t, _ :+ Reference(doc))
  }


}




