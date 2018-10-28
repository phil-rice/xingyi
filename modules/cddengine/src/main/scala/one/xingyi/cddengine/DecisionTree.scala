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
