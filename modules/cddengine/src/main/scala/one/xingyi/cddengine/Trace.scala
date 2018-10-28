package one.xingyi.cddengine
import one.xingyi.cddscenario.Scenario

case class IssueTraceData[P, R](newTree: DecisionTree[P, R], oldTree: DecisionTree[P, R], s: Scenario[P, R], oldNode: ConclusionNode[P, R], issue: DecisionIssue[P, R]) extends TraceData[P, R]
case class TraceDataWithIndex[P, R](traceData: TraceData[P, R], index: String)
sealed trait TraceData[P, R] {
  def oldTree: DecisionTree[P, R]
  def newTree: DecisionTree[P, R]
  def s: Scenario[P, R]

}
case class AddNodeTraceData[P, R](newTree: DecisionTree[P, R], decisionTreeFoldingData: DecisionTreeFoldingData[P, R]) extends TraceData[P, R] {
  def oldTree: DecisionTree[P, R] = decisionTreeFoldingData.tree
  def oldNode: ConclusionNode[P, R] = decisionTreeFoldingData.fd.conclusionNode
  def newNode: DecisionTreeNode[P, R] = decisionTreeFoldingData.newNode
  def s: Scenario[P, R] = decisionTreeFoldingData.fd.scenario
}