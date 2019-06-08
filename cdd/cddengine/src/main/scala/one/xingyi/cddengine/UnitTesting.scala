/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddengine

import one.xingyi.cddscenario.Scenario
import one.xingyi.core.misc.IdMaker
import one.xingyi.core.strings.ShortPrint
import one.xingyi.core.reflection.DefinedInSourceCodeAtLanguage._
import one.xingyi.core.language.AnyLanguage._
import scala.util.Try

trait CddRunner extends IdMaker {
  protected def engines: Try[List[Engine[_, _]]]
  protected lazy val tryTests = engines.map(engineList => NestedTest(getClass.getSimpleName, engineList.map(_.tools.test(s"Engine $getNextId"))))
}

case class ScenarioFailedException[P, R](s: Scenario[P, R], trace: TraceThroughEngineResultData[P, R], result: R)(implicit shortP: ShortPrint[P], shortR: ShortPrint[R]) extends
  RuntimeException(s"Actual result: ${shortR(result)}\n\n" +
                   s"${trace.trace.map { d => d.logic.fn.isDefinedAt(s.situation) + " " + d.logic.definedInSourceCodeAt + " " + d.logic.ifString }.mkString("\n")}\n" +
                   s"Situation ${shortP(s.situation)}\n" +
                   s"Detailed ${s.situation}")


class SimpleTestMaker[P: ShortPrint, R: ShortPrint](name: String, engine: Engine[P, R])(implicit shortPrintScenario: ShortPrint[Scenario[P, R]]) {
  val issueMap = engine.tools.issues.foldLeft(Map[Scenario[P, R], DecisionIssue[P, R]]()) { (a, d) => a + (d.scenario -> d) }
  def throwIfIssue = { s: Scenario[P, R] => issueMap.get(s).fold(s)(throw _) }
  def checkIfValid = { s: Scenario[P, R] => engine(s.situation) sideeffect (result => if (!s.acceptResult(s.situation, result)) throw ScenarioFailedException(s, engine.tools.trace(s.situation), result)) }
  def checkResult = throwIfIssue andThen checkIfValid

  def validate(engine: Engine[P, R])(s: Scenario[P, R]): CddTest = ScenarioTest(ShortPrint(s), () => checkResult(s))

  def apply = NestedTest(name, engine.tools.useCases.map(uc => NestedTest(uc.title.getOrElse("unnamed"), uc.allScenarios.map(validate(engine)))))
}
