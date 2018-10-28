package one.xingyi.cddengine
import one.xingyi.cddscenario.Scenario
import one.xingyi.core.functions.SemiGroupLanguage._

sealed trait DTFolderStrategy {
  def isDefinedAt[P, R](fd: ConclusionAndScenario[P, R]): Boolean
  def apply[P, R](fd: ConclusionAndScenario[P, R]): DecisionTreeNode[P, R]
}

trait DFFolderSimpleStrategy extends DTFolderStrategy {
  def apply[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]): DecisionTreeNode[P, R]
  def isDefinedAt[P, R](conclusionNode: ConclusionNode[P, R], scenario: Scenario[P, R]): Boolean
  def isDefinedAt[P, R](fd: ConclusionAndScenario[P, R]): Boolean = isDefinedAt(fd.conclusionNode, fd.scenario)
  def apply[P, R](fd: ConclusionAndScenario[P, R]): DecisionTreeNode[P, R] = apply(fd.conclusionNode, fd.scenario)
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
  override def isDefinedAt[P, R](fd: ConclusionAndScenario[P, R]): Boolean = fd.scenario.logic.hasCondition && fd.sAcceptsFailUsingScenarioLogic.isEmpty && !fd.conclusionNode.logic.accept(fd.scenario)
  def apply[P, R](fd: ConclusionAndScenario[P, R]) = DecisionNode(fd.scenario.logic, fd.conclusionNode.copy(scenarios = fd.sRejects), ConclusionNode(fd.sAccepts, fd.scenario.logic))
}
case object ScenariosClash extends DTFolderStrategy {
  override def isDefinedAt[P, R](fd: ConclusionAndScenario[P, R]): Boolean = fd.sAcceptsFailUsingScenarioLogic.size > 0
  override def apply[P, R](fd: ConclusionAndScenario[P, R]): DecisionTreeNode[P, R] = throw CannotAddScenarioBecauseClashes(fd.scenario, fd.conclusionNode, fd.sAcceptsFailUsingScenarioLogic)
}
