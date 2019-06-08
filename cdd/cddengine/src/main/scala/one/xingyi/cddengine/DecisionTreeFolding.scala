/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddengine

import one.xingyi.cddscenario.Scenario

import scala.util.{Success, Try}

trait DecisionTreeFolder {
  def apply[P, R]: TreeAndScenario[P, R] => DecisionTree[P, R]
  def foldFn[P, R]: (DecisionTree[P, R], Scenario[P, R]) => DecisionTree[P, R] = (t, s) => apply(TreeAndScenario(t, s))
}

case class TreeAndScenario[P, R](tree: DecisionTree[P, R], scenario: Scenario[P, R]) {
  val lens = tree.root.findLens(scenario.situation)
  val node: ConclusionNode[P, R] = (lens andThen DecisionTreeNode.nodeToConcL) (tree.root)
  val conclusionAndScenario = ConclusionAndScenario(node, scenario)
}

class SimpleDecisionTreeFolder(implicit folderStrategyFinder: DtFolderStrategyFinder, decisionTreeFoldingDataToNewTreeAndTraceData: MakeANewTree) extends DecisionTreeFolder {
  override def apply[P, R]: TreeAndScenario[P, R] => DecisionTree[P, R] =
    folderStrategyFinder.findStrategyAndApply[P, R] andThen decisionTreeFoldingDataToNewTreeAndTraceData.apply andThen (_.newTree)
}

object DecisionTreeFolder {
  implicit def folder[P, R](implicit folderStrategyFinder: DtFolderStrategyFinder): DecisionTreeFolder = new SimpleDecisionTreeFolder
}

object ConclusionAndScenario {
  def apply[P, R](treeAndScenario: TreeAndScenario[P, R]): ConclusionAndScenario[P, R] = ConclusionAndScenario(treeAndScenario.node, treeAndScenario.scenario)
}

case class ConclusionAndScenario[P, R](conclusionNode: ConclusionNode[P, R], scenario: Scenario[P, R]) {
  private def findFails(list: List[Scenario[P, R]])(code: P => R) = list.filterNot(s => Try(s.acceptResult(s.situation, code(s.situation))) == Success(true))
  lazy val (sAccepts, sRejects) = (scenario :: conclusionNode.scenarios).partition(s => scenario.logic.fn.isDefinedAt(s.situation))
  lazy val (cAccepts, cRejects) = (scenario :: conclusionNode.scenarios).partition(s => conclusionNode.logic.fn.isDefinedAt(s.situation))
  lazy val emptyScenarios = List[Scenario[P, R]]()
  lazy val sAcceptsFailUsingScenarioLogic = findFails(sAccepts)(scenario.logic.fn)
}




