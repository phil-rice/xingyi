/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddscenario
import one.xingyi.core.functions.SemiGroup
import one.xingyi.core.functions.SemiGroupLanguage._
import one.xingyi.core.reflection.{CompositeDefinedInSourceCodeAt, DefinedInSourceCodeAt, IsDefinedInSourceCodeAt, SingleDefinedInSourceCodeAt}
import one.xingyi.core.strings.ShortPrint

object ScenarioLogic {
  def empty[P, R] = NoScenarioLogic[P, R](DefinedInSourceCodeAt.definedInSourceCodeAt(2), "<empty>")
  def toSeq[P, R]: ScenarioLogic[P, R] => Seq[SingleScenarioLogic[P, R]] = {case s: SingleScenarioLogic[P, R] => List(s); case CompositeScenarioLogic(defined) => defined}
  implicit def semiGroupForScenarioLogic[P, R]: SemiGroup[ScenarioLogic[P, R]] = (t1, t2) => CompositeScenarioLogic(toSeq(t1) ++ toSeq(t2))
  implicit def isDefinedInSourceCodeAt[P, R]: IsDefinedInSourceCodeAt[ScenarioLogic[P, R]] = {
    case s: SingleScenarioLogic[P, R] => s.definedInSourceCodeAt
    case CompositeScenarioLogic(logics) => CompositeDefinedInSourceCodeAt(logics.map(_.definedInSourceCodeAt))
  }
}

trait ScenarioLogic[P, R] {
  def ifString: String
  def fn: PartialFunction[P, R]
  def hasCondition: Boolean
  def accept(s: Scenario[P, R]) = fn.isDefinedAt(s.situation) && s.acceptResult(s.situation, fn.apply(s.situation))
}


trait SingleScenarioLogic[P, R] extends ScenarioLogic[P, R] {
  //arrgh... cake pattern!!! But for once it saves a lot of code here
  def definedInSourceCodeAt: SingleDefinedInSourceCodeAt
  override def toString: String = s"${getClass.getSimpleName}@($definedInSourceCodeAt)"
}

case class NoScenarioLogic[P, R](definedInSourceCodeAt: SingleDefinedInSourceCodeAt, ifString: String) extends SingleScenarioLogic[P, R] {
  override def fn: PartialFunction[P, R] = {case p if false => throw new RuntimeException("should never happen")}
  override def hasCondition: Boolean = false
}

case class ResultScenarioLogic[P, R](result: R, definedInSourceCodeAt: SingleDefinedInSourceCodeAt, ifString: String) extends SingleScenarioLogic[P, R] {
  override val fn: PartialFunction[P, R] = {case a => result}
  override def hasCondition: Boolean = false
}
case class BecauseScenarioLogic[P, R](fn: PartialFunction[P, R], definedInSourceCodeAt: SingleDefinedInSourceCodeAt, ifString: String)(implicit shortPrintP: ShortPrint[P], shortPrintR: ShortPrint[R]) extends SingleScenarioLogic[P, R] {
  override def hasCondition: Boolean = true
}

case class WhenResultScenarioLogic[P, R](when: P => Boolean, result: R, definedInSourceCodeAt: SingleDefinedInSourceCodeAt, ifString: String)(implicit shortPrintP: ShortPrint[P], shortPrintR: ShortPrint[R]) extends SingleScenarioLogic[P, R] {
  override val fn: PartialFunction[P, R] = {case p if when(p) => result}
  override def hasCondition: Boolean = true
}

case class WhenCodeScenarioLogic[P, R](when: P => Boolean, code: P => R, definedInSourceCodeAt: SingleDefinedInSourceCodeAt, ifString: String)(implicit shortPrintP: ShortPrint[P], shortPrintR: ShortPrint[R]) extends SingleScenarioLogic[P, R] {
  override val fn: PartialFunction[P, R] = {case p if when(p) => code(p)}
  override def hasCondition: Boolean = true
}


case class CompositeScenarioLogic[P, R](logics: Seq[SingleScenarioLogic[P, R]]) extends ScenarioLogic[P, R] {
  override def hasCondition: Boolean = logics.exists(_.hasCondition)
  override lazy val fn: PartialFunction[P, R] =  logics.map(_.fn).orAll
  override def toString(): String = s"SCompLogic(${logics.mkString(",")})"
  override def ifString: String = logics.map(_.ifString).mkString(" or ")
}
