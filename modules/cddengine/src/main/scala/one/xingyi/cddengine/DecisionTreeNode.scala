/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddengine

import one.xingyi.cddscenario.{Scenario, ScenarioLogic}
import one.xingyi.core.language.AnyLanguage._
import one.xingyi.core.optics.Lens
import one.xingyi.core.reflection.{DefinedInSourceCodeAt, IsDefinedInSourceCodeAt}

sealed trait DecisionTreeNode[P, R] {
  def logic: ScenarioLogic[P, R]
  def findLens(p: P): Lens[DecisionTreeNode[P, R], DecisionTreeNode[P, R]]
  def findLensAndCnLens = use(findLens)(lens => (lens, lens andThen DecisionTreeNode.nodeToConcL))
  def fold[Acc](fold: DecisionTreeNodeFold[Acc, P, R]): Acc

}

object DecisionTreeNodeFold {
}
trait DecisionTreeNodeFold[Acc, P, R] {
  def concFn: ConclusionNode[P, R] => Acc
  def decFn: DecisionNode[P, R] => Acc
}

object ConclusionNode {
  implicit def isDefined[P, R]: IsDefinedInSourceCodeAt[ConclusionNode[P, R]] = { c => DefinedInSourceCodeAt(c.logic) }
}
case class ConclusionNode[P, R](scenarios: List[Scenario[P, R]], logic: ScenarioLogic[P, R]) extends DecisionTreeNode[P, R] {
  def accept(s: Scenario[P, R]) = logic.accept(s)
  override def findLens(p: P): Lens[DecisionTreeNode[P, R], DecisionTreeNode[P, R]] = Lens.identity
  override def fold[Acc](fold: DecisionTreeNodeFold[Acc, P, R]): Acc = fold.concFn(this)
}

object DecisionNode {
  implicit def isDefined[P, R]: IsDefinedInSourceCodeAt[DecisionNode[P, R]] = { d => DefinedInSourceCodeAt(d.logic) }
}

case class DecisionNode[P, R](logic: ScenarioLogic[P, R], ifFalse: DecisionTreeNode[P, R], ifTrue: DecisionTreeNode[P, R]) extends DecisionTreeNode[P, R] {
  import DecisionTreeNode._
  override def findLens(p: P): Lens[DecisionTreeNode[P, R], DecisionTreeNode[P, R]] = logic.fn.isDefinedAt(p) match {
    case true => nodeToDNL andThen DNLtoTrue[P, R] andThen ifTrue.findLens(p)
    case false => nodeToDNL andThen DNLtoFalse[P, R] andThen ifFalse.findLens(p)
  }
  override def fold[Acc](fold: DecisionTreeNodeFold[Acc, P, R]): Acc = fold.decFn(this)
}

//class DefaultNotSpecifiedException[P](p: P) extends RuntimeException(s"Default has not been specified")

object DecisionTreeNode {
//  def defaultFn[P, R]: P => R = { p: P => throw new DefaultNotSpecifiedException(p) }
  def nodeToDNL[P, R] = Lens.cast[DecisionTreeNode[P, R], DecisionNode[P, R]]
  def nodeToConcL[P, R] = Lens.cast[DecisionTreeNode[P, R], ConclusionNode[P, R]]
  def DNLtoFalse[P, R] = Lens[DecisionNode[P, R], DecisionTreeNode[P, R]](_.ifFalse, (dn, c) => dn.copy(ifFalse = c))
  def DNLtoTrue[P, R] = Lens[DecisionNode[P, R], DecisionTreeNode[P, R]](_.ifTrue, (dn, c) => dn.copy(ifTrue = c))
}

sealed abstract class DecisionIssue[P, R] (msg: String)extends RuntimeException(msg){
  def conclusionNode: ConclusionNode[P,R]
  def scenario: Scenario[P, R]
}
case class CannotAddScenarioBecauseClashes[P, R](scenario: Scenario[P, R], conclusionNode: ConclusionNode[P,R], clashesWith: List[Scenario[P, R]]) extends
  DecisionIssue[P,R](
    s"""Cannot add scenario\n${scenario.logic.definedInSourceCodeAt} $scenario
       |To conclusion node $conclusionNode
       |Because it clashes with
       |${clashesWith.map(s => s"${s.logic.definedInSourceCodeAt} $s").mkString("\n")}
       | """.stripMargin)

case class DecisionTree[P, R](root: DecisionTreeNode[P, R], issues: List[DecisionIssue[P, R]])

object DecisionTree {
  def empty[P, R]: DecisionTree[P, R] = DecisionTree(ConclusionNode(List(), ScenarioLogic.empty), List())

  def findWithParents[P, R](tree: DecisionTree[P, R], situation: P) = tree.root.fold(new DecisionTreeNodeFold[List[DecisionTreeNode[P, R]], P, R] {
    override def concFn: ConclusionNode[P, R] => List[DecisionTreeNode[P, R]] = List(_)
    override def decFn: DecisionNode[P, R] => List[DecisionTreeNode[P, R]] = d => if (d.logic.fn.isDefinedAt(situation)) d :: d.ifTrue.fold(this) else d :: d.ifFalse.fold(this)
  })
  def findWithParentsForScenario[P, R](tree: DecisionTree[P, R])(s: Scenario[P, R]) = findWithParents[P, R](tree, s.situation)
}


trait DefaultFunction[P, R] extends PartialFunction[P, R]

case class SimpleDefaultFunction[P, R](fn: P => R) extends DefaultFunction[P, R] {
  override def isDefinedAt(x: P): Boolean = true
  override def apply(v1: P): R = fn(v1)
}

