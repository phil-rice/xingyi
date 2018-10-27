/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddengine

import one.xingyi.cddscenario.Scenario

sealed trait ValidationIssues[P, R]
case class ScenarioComesToWrongConclusion[P, R](s: Scenario[P, R], actualResult: R) extends ValidationIssues[P, R]

case class ValidationReport[P, R](engine: Engine[P, R], issues: Seq[ValidationIssues[P, R]])
trait Validation[P, R] extends (Engine[P, R] => ValidationReport[P, R])

class SimpleValidation[P, R] extends Validation[P, R] {
  override def apply(e: Engine[P, R]): ValidationReport[P, R] =
    ValidationReport(e, e.tools.scenarios.map(s => (s, e.apply(s.situation))).collect { case (s, r) if s.result.isDefined && Some(r) != s.result =>
      val result = ScenarioComesToWrongConclusion(s, r)
      println(s" s is [${s.situation}]\n Actual is    $r\nExpected is ${s.result} result: $result")
      result
    })
}
