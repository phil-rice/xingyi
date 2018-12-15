/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddengine
import one.xingyi.cddscenario.Scenario
import one.xingyi.core.functions.SemiGroupLanguage._

sealed trait DTFolderStrategy {
  def isDefinedAt[P, R](fd: ConclusionAndScenario[P, R]): Boolean
  def apply[P, R](fd: ConclusionAndScenario[P, R]): Either[DecisionIssue[P, R], DecisionTreeNode[P, R]]
}

trait DFFolderSimpleStrategy extends DTFolderStrategy {
  def apply[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]): Either[DecisionIssue[P, R], DecisionTreeNode[P, R]]
  def isDefinedAt[P, R](conclusionNode: ConclusionNode[P, R], scenario: Scenario[P, R]): Boolean
  def isDefinedAt[P, R](fd: ConclusionAndScenario[P, R]): Boolean = isDefinedAt(fd.conclusionNode, fd.scenario)
  def apply[P, R](fd: ConclusionAndScenario[P, R]): Either[DecisionIssue[P, R], DecisionTreeNode[P, R]] = apply(fd.conclusionNode, fd.scenario)
}

case object NullOp extends DFFolderSimpleStrategy {
  override def isDefinedAt[P, R](conclusionNode: ConclusionNode[P, R], scenario: Scenario[P, R]): Boolean = false
  override def apply[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]) = throw new IllegalStateException("should not be called")
}

case object AddScenarioToEmptyConclusion extends DFFolderSimpleStrategy {
  def isDefinedAt[P, R](conclusionNode: ConclusionNode[P, R], scenario: Scenario[P, R]): Boolean = conclusionNode.scenarios.isEmpty
  def apply[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]) = Right(c.copy(scenarios = c.scenarios :+ s, logic = s.logic))
}
case object AddScenarioToConclusion extends DFFolderSimpleStrategy {
  def isDefinedAt[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]): Boolean = c.accept(s)
  def apply[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]) = Right(c.copy(scenarios = c.scenarios :+ s))
}
case object AddScenarioReplaceLogic extends DFFolderSimpleStrategy {
  override def isDefinedAt[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]): Boolean = !c.logic.hasCondition && s.logic.hasCondition && c.scenarios.forall(s.logic.accept)
  def apply[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]) = Right(c.copy(scenarios = c.scenarios :+ s, logic = s.logic))
}
case object AddScenarioMergeCondition extends DFFolderSimpleStrategy {
  def isDefinedAt[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]): Boolean = c.logic.hasCondition && s.logic.hasCondition && c.accept(s)
  def apply[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]) = Right(c.copy(scenarios = c.scenarios :+ s, logic = c.logic or s.logic))
}
case object MakeDecisionNodeScenarioAsFalse extends DFFolderSimpleStrategy {
  def isDefinedAt[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]): Boolean = c.logic.hasCondition && !c.logic.fn.isDefinedAt(s.situation) && !c.accept(s)
  def apply[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]) = Right(DecisionNode(c.logic, ConclusionNode(List(s), s.logic), c))
}
case object MakeDecisionNodeScenarioAsTrue extends DTFolderStrategy {
  override def isDefinedAt[P, R](fd: ConclusionAndScenario[P, R]): Boolean = fd.scenario.logic.hasCondition && fd.sAcceptsFailUsingScenarioLogic.isEmpty && !fd.conclusionNode.logic.accept(fd.scenario)
  def apply[P, R](fd: ConclusionAndScenario[P, R]) = Right(DecisionNode(fd.scenario.logic, fd.conclusionNode.copy(scenarios = fd.sRejects), ConclusionNode(fd.sAccepts, fd.scenario.logic)))
}
case object ScenariosClash extends DTFolderStrategy {
  override def isDefinedAt[P, R](fd: ConclusionAndScenario[P, R]): Boolean = fd.sAcceptsFailUsingScenarioLogic.size > 0
  override def apply[P, R](fd: ConclusionAndScenario[P, R]) = Left(CannotAddScenarioBecauseClashes(fd.scenario, fd.conclusionNode, fd.sAcceptsFailUsingScenarioLogic))
}
