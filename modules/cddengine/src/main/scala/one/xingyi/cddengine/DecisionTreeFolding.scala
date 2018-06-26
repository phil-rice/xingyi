/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddengine

import one.xingyi.cddscenario.Scenario
import one.xingyi.core.optics.Lens
import one.xingyi.core.functions.SemiGroupLanguage._
import scala.util.{Failure, Success, Try}

case class DecisionTreeFoldingData[P, R](st: DTFolderStrategy, lens: Lens[DecisionTreeNode[P, R], DecisionTreeNode[P, R]], fd: FolderData[P, R]) {
  val newNode = st(fd)
}

case class TraceDataWithIndex[P,R](traceData: TraceData[P,R], index: String)
sealed trait TraceData[P, R] {
  def tree: DecisionTree[P, R]
  def s: Scenario[P, R]

}
case class AddNodeTraceData[P, R](tree: DecisionTree[P, R], s: Scenario[P, R], st: DTFolderStrategy, oldNode: ConclusionNode[P, R], newNode: DecisionTreeNode[P, R]) extends TraceData[P, R]
case class IssueTraceData[P, R](tree: DecisionTree[P, R], s: Scenario[P, R], oldNode: ConclusionNode[P, R], issue: DecisionIssue[P, R]) extends TraceData[P, R]


object DecisionTreeFolder {
  def findStrategy[P, R](tree: DecisionTree[P, R], s: Scenario[P, R])(implicit folderStrategyFinder: DtFolderStrategyFinder[P, R]): Try[DecisionTreeFoldingData[P, R]] = {
    val (lens, lensCn) = tree.root.findLensAndCnLens(s.situation)
    val node: ConclusionNode[P, R] = lensCn(tree.root)
    val fd = FolderData(node, s)
    Try(folderStrategyFinder(fd)).map(s => DecisionTreeFoldingData(s, lens, fd))
  }

  def applyStrategy[P, R](tree: DecisionTree[P, R])(decisionTreeFoldingData: DecisionTreeFoldingData[P, R]) =
    DecisionTree(decisionTreeFoldingData.lens.set(tree.root, decisionTreeFoldingData.newNode), tree.issues)


  def recordError[P, R](tree: DecisionTree[P, R]): Throwable => DecisionTree[P, R] = {
    case e: DecisionIssue[_, _] => DecisionTree(tree.root, tree.issues :+ e.asInstanceOf[DecisionIssue[P, R]])
    case e => throw e
  }

  def findNewTree[P, R](tree: DecisionTree[P, R])(tryD: Try[DecisionTreeFoldingData[P, R]]) = tryD.map(applyStrategy(tree)).fold(recordError(tree), t => t)

  implicit def folder[P, R](implicit folderStrategyFinder: DtFolderStrategyFinder[P, R]): DecisionTreeFolder[P, R] = {
    (tree, s) => findNewTree(tree)(findStrategy[P, R](tree, s))
  }

  def apply[P, R](list: List[Scenario[P, R]])(implicit decisionTreeFolder: DecisionTreeFolder[P, R], default: DefaultFunction[P, R]): DecisionTree[P, R] =
    list.foldLeft[DecisionTree[P, R]](DecisionTree.empty)(decisionTreeFolder)

  def trace[P, R](list: List[Scenario[P, R]])(implicit validation: Validation[P, R], decisionTreeFolder: DecisionTreeFolder[P, R]): List[TraceData[P, R]] =
    list.zipWithIndex.foldLeft[List[TraceData[P, R]]](List()) {
      case (acc, (s, i)) =>
        val tree = acc.headOption.fold(DecisionTree.empty[P, R])(t => t.tree)
        findStrategy(tree, s) match {
          case Success(d) =>
            val st: DTFolderStrategy = d.st
            val newTree: DecisionTree[P, R] = applyStrategy(tree)(d)
            val v = validation(Engine1(newTree, list.take(i), List()))
            AddNodeTraceData(newTree, s, st, d.fd.conclusionNode, d.newNode) :: acc
          case Failure(e: DecisionIssue[_, _]) =>
            val d = e.asInstanceOf[DecisionIssue[P, R]]
            IssueTraceData[P, R](recordError(tree)(e), s, d.conclusionNode, d) :: acc
          case Failure(e) => throw e
        }

    }


}
trait DecisionTreeFolder[P, R] extends ((DecisionTree[P, R], Scenario[P, R]) => DecisionTree[P, R])


object FolderData {
  def create[P, R]: (ConclusionNode[P, R], Scenario[P, R]) => FolderData[P, R] = (c, s) => new FolderData(c, s)
}

case class FolderData[P, R](conclusionNode: ConclusionNode[P, R], scenario: Scenario[P, R]) {
  private def findFails(list: List[Scenario[P, R]])(code: P => R) = list.filterNot(s => Try(s.acceptResult(s.situation, code(s.situation))) == Success(true))
  lazy val (sAccepts, sRejects) = (scenario :: conclusionNode.scenarios).partition(s => scenario.logic.fn.isDefinedAt(s.situation))
  lazy val (cAccepts, cRejects) = (scenario :: conclusionNode.scenarios).partition(s => conclusionNode.logic.fn.isDefinedAt(s.situation))
  lazy val emptyScenarios = List[Scenario[P, R]]()
  lazy val sAcceptsFailUsingScenarioLogic = findFails(sAccepts)(scenario.logic.fn)
}


sealed trait DTFolderStrategy {
  def isDefinedAt[P, R](fd: FolderData[P, R]): Boolean
  def apply[P, R](fd: FolderData[P, R]): DecisionTreeNode[P, R]

}
trait DFFolderSimpleStrategy extends DTFolderStrategy {
  def apply[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]): DecisionTreeNode[P, R]
  def isDefinedAt[P, R](conclusionNode: ConclusionNode[P, R], scenario: Scenario[P, R]): Boolean
  def isDefinedAt[P, R](fd: FolderData[P, R]): Boolean = isDefinedAt(fd.conclusionNode, fd.scenario)
  def apply[P, R](fd: FolderData[P, R]): DecisionTreeNode[P, R] = apply(fd.conclusionNode, fd.scenario)
}

case object NullOp extends DFFolderSimpleStrategy {
  override def isDefinedAt[P, R](conclusionNode: ConclusionNode[P, R], scenario: Scenario[P, R]): Boolean = false
  override def apply[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]): DecisionTreeNode[P, R] = throw new IllegalStateException("should not be called")
}

case object AddScenarioToEmptyConclusion extends DFFolderSimpleStrategy {
  def isDefinedAt[P, R](conclusionNode: ConclusionNode[P, R], scenario: Scenario[P, R]): Boolean = conclusionNode.scenarios.isEmpty
  def apply[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]): DecisionTreeNode[P, R] = c.copy(scenarios = c.scenarios :+ s, logic = s.logic)
}
case object AddScenarioToConclusion extends DFFolderSimpleStrategy {
  def isDefinedAt[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]): Boolean = c.accept(s)
  def apply[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]): DecisionTreeNode[P, R] = c.copy(scenarios = c.scenarios :+ s)
}
case object AddScenarioReplaceLogic extends DFFolderSimpleStrategy {
  override def isDefinedAt[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]): Boolean = !c.logic.hasCondition && s.logic.hasCondition && c.scenarios.forall(s.logic.accept)
  def apply[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]): DecisionTreeNode[P, R] = c.copy(scenarios = c.scenarios :+ s, logic = s.logic)
}
case object AddScenarioMergeCondition extends DFFolderSimpleStrategy {
  def isDefinedAt[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]): Boolean = c.logic.hasCondition && s.logic.hasCondition && c.accept(s)
  def apply[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]): DecisionTreeNode[P, R] = c.copy(scenarios = c.scenarios :+ s, logic = c.logic or s.logic)
}
case object MakeDecisionNodeScenarioAsFalse extends DFFolderSimpleStrategy {
  def isDefinedAt[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]): Boolean = c.logic.hasCondition && !c.logic.fn.isDefinedAt(s.situation) && !c.accept(s)
  def apply[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]): DecisionTreeNode[P, R] = DecisionNode(c.logic, ConclusionNode(List(s), s.logic), c)
}
case object MakeDecisionNodeScenarioAsTrue extends DTFolderStrategy {
  override def isDefinedAt[P, R](fd: FolderData[P, R]): Boolean = fd.scenario.logic.hasCondition && fd.sAcceptsFailUsingScenarioLogic.isEmpty && !fd.conclusionNode.logic.accept(fd.scenario)
  def apply[P, R](fd: FolderData[P, R]) = DecisionNode(fd.scenario.logic, fd.conclusionNode.copy(scenarios = fd.sRejects), ConclusionNode(fd.sAccepts, fd.scenario.logic))
}
case object ScenariosClash extends DTFolderStrategy {
  override def isDefinedAt[P, R](fd: FolderData[P, R]): Boolean = fd.sAcceptsFailUsingScenarioLogic.size > 0
  override def apply[P, R](fd: FolderData[P, R]): DecisionTreeNode[P, R] = throw  CannotAddScenarioBecauseClashes(fd.scenario,fd.conclusionNode, fd.sAcceptsFailUsingScenarioLogic)
}


object DtFolderStrategyFinder {
  implicit def defaultFinder[P, R]: DtFolderStrategyFinder[P, R] = new SimpleDtFolderStrategyFinder[P, R]
}

trait DtFolderStrategyFinder[P, R] extends (FolderData[P, R] => DTFolderStrategy)

class SimpleDtFolderStrategyFinder[P, R] extends DtFolderStrategyFinder[P, R] {

  type SS = PartialFunction[(ConclusionNode[P, R], Scenario[P, R]), DTFolderStrategy]

  val folders = List[DTFolderStrategy](
    AddScenarioToEmptyConclusion,
    AddScenarioReplaceLogic,
    AddScenarioMergeCondition,
    AddScenarioToConclusion,
    MakeDecisionNodeScenarioAsFalse,
    MakeDecisionNodeScenarioAsTrue,
    ScenariosClash)

  def apply(fd: FolderData[P, R]): DTFolderStrategy = folders.find(_.isDefinedAt(fd)).getOrElse(throw new RuntimeException(s"Cannot work out how to deal with $fd"))
}

