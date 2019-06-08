/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddscenario

import one.xingyi.core.reflection.IsDefinedInSourceCodeAt
import one.xingyi.core.strings.ShortPrint

import scala.language.higherKinds

trait HasScenarios[T[_, _]] {
  def allScenarios[P, R](t: T[P, R]): List[Scenario[P, R]]
}

object Scenario {
  implicit def scenarioIsDefined[P, R]: IsDefinedInSourceCodeAt[Scenario[P, R]] = _.data.definedInSourceCodeAt
  implicit def scenarioData[P, R]: HasEngineComponentData[Scenario[P, R]] = s => s.data
  implicit def shortPrintFor[P: ShortPrint, R: ShortPrint]: ShortPrint[Scenario[P, R]] = s => s"${ShortPrint(s.situation)} => ${s.result.fold("")(ShortPrint.apply)}"
}
case class Scenario[P, R](situation: P, result: Option[R], logic: SingleScenarioLogic[P, R], assertions: List[ScenarioAssertion[P, R]], data: EngineComponentData) {
  def acceptResult(p: P, r: R) = result.fold(true)(_ == r) && assertions.forall(_.isTrue(p, r))
}


