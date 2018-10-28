package one.xingyi.cddengine
import one.xingyi.cddscenario.Scenario

case class IssueTraceData[P, R](tree: DecisionTree[P, R], s: Scenario[P, R], oldNode: ConclusionNode[P, R], issue: DecisionIssue[P, R]) extends TraceData[P, R]

case class TraceDataWithIndex[P, R](traceData: TraceData[P, R], index: String)
sealed trait TraceData[P, R] {
  def tree: DecisionTree[P, R]
  def s: Scenario[P, R]

}
case class AddNodeTraceData[P, R](tree: DecisionTree[P, R], decisionTreeFoldingData: DecisionTreeFoldingData[P, R]) extends TraceData[P, R] {
  val oldNode: ConclusionNode[P, R] = decisionTreeFoldingData.fd.conclusionNode
  val newNode: DecisionTreeNode[P, R] = decisionTreeFoldingData.newNode
  override def s: Scenario[P, R] = decisionTreeFoldingData.fd.scenario
}