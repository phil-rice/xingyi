/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddengine
import one.xingyi.cddscenario.{Scenario, ScenarioLogic}

case class DecisionTree[P, R](root: DecisionTreeNode[P, R], issues: List[DecisionIssue[P, R]])
object DecisionTree {
  def empty[P, R]: DecisionTree[P, R] = DecisionTree(ConclusionNode(List(), ScenarioLogic.empty), List())

  def findWithParents[P, R](tree: DecisionTree[P, R], situation: P) = tree.root.fold(new DecisionTreeNodeFold[List[DecisionTreeNode[P, R]], P, R] {
    override def concFn: ConclusionNode[P, R] => List[DecisionTreeNode[P, R]] = List(_)
    override def decFn: DecisionNode[P, R] => List[DecisionTreeNode[P, R]] = d => if (d.logic.fn.isDefinedAt(situation)) d :: d.ifTrue.fold(this) else d :: d.ifFalse.fold(this)
  })
  def findWithParentsForScenario[P, R](tree: DecisionTree[P, R])(s: Scenario[P, R]) = findWithParents[P, R](tree, s.situation)
}
