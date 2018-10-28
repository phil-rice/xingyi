/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddengine

import one.xingyi.cddengine.DecisionTreeFolder.{applyStrategy, recordError}
import one.xingyi.cddscenario.Scenario
import one.xingyi.core.optics.Lens

import scala.util.{Failure, Success, Try}


case class DecisionTreeFoldingData[P, R](tree: DecisionTree[P, R], st: DTFolderStrategy, lens: Lens[DecisionTreeNode[P, R], DecisionTreeNode[P, R]], fd: ConclusionAndScenario[P, R]) {
  val newNode = st(fd)

}

object DecisionTreeFolder {

  def applyStrategy[P, R](tree: DecisionTree[P, R])(decisionTreeFoldingData: DecisionTreeFoldingData[P, R]) =
    DecisionTree(decisionTreeFoldingData.lens.set(tree.root, decisionTreeFoldingData.newNode), tree.issues)


  def recordError[P, R](tree: DecisionTree[P, R]): Throwable => DecisionTree[P, R] = {
    case e: DecisionIssue[_, _] => DecisionTree(tree.root, tree.issues :+ e.asInstanceOf[DecisionIssue[P, R]])
    case e => throw e
  }

  implicit def folder[P, R](implicit folderStrategyFinder: DtFolderStrategyFinder): DecisionTreeFolder = new SimpleDecisionTreeFolder

  def apply[P, R](list: List[Scenario[P, R]])(implicit decisionTreeFolder: DecisionTreeFolder, default: DefaultFunction[P, R]): DecisionTree[P, R] =
    list.foldLeft[DecisionTree[P, R]](DecisionTree.empty)(decisionTreeFolder.apply)

  def trace[P, R](list: Seq[Scenario[P, R]])(implicit strategy: DtFolderStrategyFinder): List[TraceData[P, R]] =
    list.zipWithIndex.foldLeft[List[TraceData[P, R]]](List()) {
      case (acc, (s, i)) =>
        val tree = acc.headOption.fold(DecisionTree.empty[P, R])(t => t.newTree)
        strategy.findStrategy(tree, s) match {
          case Success(d) =>
            val st: DTFolderStrategy = d.st
            val newTree: DecisionTree[P, R] = applyStrategy(tree)(d)
            AddNodeTraceData(newTree, d) :: acc
          case Failure(e: DecisionIssue[_, _]) =>
            val d = e.asInstanceOf[DecisionIssue[P, R]]
            IssueTraceData[P, R](recordError(tree)(e), tree, s, d.conclusionNode, d) :: acc
          case Failure(e) => throw e
        }
    }
}
trait DecisionTreeFolder {
  def apply[P, R](tree: DecisionTree[P, R], scenario: Scenario[P, R]): DecisionTree[P, R]
}

class SimpleDecisionTreeFolder(implicit folderStrategyFinder: DtFolderStrategyFinder) extends DecisionTreeFolder {
  override def apply[P, R](tree: DecisionTree[P, R], scenario: Scenario[P, R]): DecisionTree[P, R] =
    folderStrategyFinder.findStrategy[P, R](tree, scenario) map applyStrategy(tree) fold(recordError(tree), t => t)
}


case class ConclusionAndScenario[P, R](conclusionNode: ConclusionNode[P, R], scenario: Scenario[P, R]) {
  private def findFails(list: List[Scenario[P, R]])(code: P => R) = list.filterNot(s => Try(s.acceptResult(s.situation, code(s.situation))) == Success(true))
  lazy val (sAccepts, sRejects) = (scenario :: conclusionNode.scenarios).partition(s => scenario.logic.fn.isDefinedAt(s.situation))
  lazy val (cAccepts, cRejects) = (scenario :: conclusionNode.scenarios).partition(s => conclusionNode.logic.fn.isDefinedAt(s.situation))
  lazy val emptyScenarios = List[Scenario[P, R]]()
  lazy val sAcceptsFailUsingScenarioLogic = findFails(sAccepts)(scenario.logic.fn)
}

object DtFolderStrategyFinder {
  implicit val defaultFinder: DtFolderStrategyFinder = new SimpleDtFolderStrategyFinder
}

trait DtFolderStrategyFinder {
  def apply[P, R](conclusionAndScenario: ConclusionAndScenario[P, R]): DTFolderStrategy
  def findStrategy[P, R](tree: DecisionTree[P, R], s: Scenario[P, R])(implicit folderStrategyFinder: DtFolderStrategyFinder): Try[DecisionTreeFoldingData[P, R]] = {
    val (lens, lensCn) = tree.root.findLensAndCnLens(s.situation)

    val node: ConclusionNode[P, R] = lensCn(tree.root)
    val fd = ConclusionAndScenario(node, s)
    Try(folderStrategyFinder(fd)).map(s => DecisionTreeFoldingData(tree, s, lens, fd))
  }

}
class SimpleDtFolderStrategyFinder extends DtFolderStrategyFinder {
  val folders = List[DTFolderStrategy](
    AddScenarioToEmptyConclusion,
    AddScenarioReplaceLogic,
    AddScenarioMergeCondition,
    AddScenarioToConclusion,
    MakeDecisionNodeScenarioAsFalse,
    MakeDecisionNodeScenarioAsTrue,
    ScenariosClash)

  def apply[P, R](fd: ConclusionAndScenario[P, R]): DTFolderStrategy = folders.find(_.isDefinedAt(fd)).getOrElse(throw new RuntimeException(s"Cannot work out how to deal with $fd"))


}

