/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddengine
import one.xingyi.cddscenario.Scenario
import one.xingyi.core.language.AnyLanguage._
import one.xingyi.core.misc.IdMaker
import one.xingyi.core.strings.ShortPrint

import scala.util.Try

trait CddRunner extends IdMaker {
  protected def engines: Try[List[Engine[_, _]]]
  protected lazy val tryTests = engines.map(engineList => NestedTest(getClass.getSimpleName, engineList.map(_.tools.test(s"a engine $getNextId"))))
}

case class ScenarioFailedException[R](s: Scenario[_, _], result: R)(implicit short: ShortPrint[R]) extends RuntimeException(s"Actual result: ${short(result)}")


class SimpleTestMaker[P, R](name: String, engine: Engine[P, R])(implicit shortPrintScenario: ShortPrint[Scenario[P, R]]) {
  def checkResult(s: Scenario[P, R]) = engine(s.situation) sideeffect (result => if (!s.acceptResult(s.situation, result)) throw new ScenarioFailedException(s, result))

  def validate(engine: Engine[P, R])(s: Scenario[P, R]): CddTest = ScenarioTest(ShortPrint(s), () => checkResult(s))

  def apply = NestedTest(name, engine.tools.useCases.map(uc => NestedTest(uc.title.getOrElse("unnamed"), uc.allScenarios.map(validate(engine)))))
}
