/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddscenario
import one.xingyi.core.builder.{No, Yes}
import one.xingyi.core.reflection.DefinedInSourceCodeAt

import scala.language.experimental.macros
import scala.language.higherKinds
import scala.reflect.macros.blackbox

object EngineBuilderLanguage2 {

  def when_impl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, R: c.WeakTypeTag, HasResult: c.WeakTypeTag, HasCode: c.WeakTypeTag](c: blackbox.Context)(when: c.Expr[(P1, P2) => Boolean]): c.Expr[ScBuilder[(P1, P2), R, HasResult, Yes, HasCode, No]] = {
    import c.universe._
    reify {
      val hasBuilder = (c.Expr[HasScBuilder[(P1, P2), R, HasResult, No, HasCode, No]](c.prefix.tree)).splice
      EngineBuilderLanguage.withWhenPrim(hasBuilder.builder, when.splice.tupled, c.literal(show(when.tree)).splice)
    }
  }
  def code_impl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, R: c.WeakTypeTag, HasResult: c.WeakTypeTag, HasWhen: c.WeakTypeTag](c: blackbox.Context)(code: c.Expr[(P1, P2) => R]): c.Expr[ScBuilder[(P1, P2), R, HasResult, HasWhen, Yes, No]] = {
    import c.universe._
    reify {
      val hasBuilder = (c.Expr[HasScBuilder[(P1, P2), R, HasResult, HasWhen, No, No]](c.prefix.tree)).splice
      EngineBuilderLanguage.withCodePrim(hasBuilder.builder, code.splice.tupled, c.literal(show(code.tree)).splice)
    }
  }
  def because_impl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, R: c.WeakTypeTag, HasResult: c.WeakTypeTag](c: blackbox.Context)(becauseFn: c.Expr[PartialFunction[(P1, P2), R]]): c.Expr[ScBuilder[(P1, P2), R, HasResult, No, No, Yes]] = {
    import c.universe._
    reify {
      val hasBuilder = (c.Expr[HasScBuilder[(P1, P2), R, HasResult, No, No, No]](c.prefix.tree)).splice
      EngineBuilderLanguage.withBecausePrim(hasBuilder.builder, becauseFn.splice, c.literal(show(becauseFn.tree)).splice, c.literal(show(becauseFn.tree)).splice)

    }
  }
}
trait EngineBuilderLanguage2 extends EngineBuilderLanguage {
  protected def scenario[P1, P2, R](p1: P1, p2: P2)(implicit scenarioAggregator: ScenarioAggregator2[(P1, P2), R]) =
    new ScBuilder[(P1, P2), R, No, No, No, No](getNextId, (p1, p2), EngineComponentData(DefinedInSourceCodeAt.definedInSourceCodeAt(4), None), None, None, None, None, "", "")

  implicit protected class ScBuilderAddResultOps[P1, P2, R, HasWhen](val builder: ScBuilder[(P1, P2), R, No, HasWhen, No, No]) extends HasScBuilder[(P1, P2), R, No, HasWhen, No, No] {
    def produces(r: R): ScBuilder[(P1, P2), R, Yes, HasWhen, No, No] = builder.withResultPrim(r)
  }
  implicit protected class ScBuilderAddWhenOps[P1, P2, R, HasResult, HasCode](val builder: ScBuilder[(P1, P2), R, HasResult, No, HasCode, No]) extends HasScBuilder[(P1, P2), R, HasResult, No, HasCode, No] {
    def when(when: (P1, P2) => Boolean): ScBuilder[(P1, P2), R, HasResult, Yes, HasCode, No] = macro EngineBuilderLanguage2.when_impl[P1, P2, R, HasResult, HasCode]
  }
  implicit protected class ScBuilderAddCodeOps[P1, P2, R, HasResult, HasWhen](val builder: ScBuilder[(P1, P2), R, HasResult, HasWhen, No, No]) extends HasScBuilder[(P1, P2), R, HasResult, HasWhen, No, No] {
    def  code(code: (P1, P2) => R): ScBuilder[(P1, P2), R, HasResult, HasWhen, Yes, No] = macro EngineBuilderLanguage2.code_impl[P1, P2, R, HasResult, HasWhen]
  }
  implicit protected class ScBuilderAddBecauseOps[P1, P2, R, HasResult](val builder: ScBuilder[(P1, P2), R, HasResult, No, No, No]) extends HasScBuilder[(P1, P2), R, HasResult, No, No, No] {
    def because(becauseFn: PartialFunction[(P1, P2), R]): ScBuilder[(P1, P2), R, HasResult, No, No, Yes] = macro EngineBuilderLanguage1.because_impl[(P1, P2), R, HasResult]
  }
}
