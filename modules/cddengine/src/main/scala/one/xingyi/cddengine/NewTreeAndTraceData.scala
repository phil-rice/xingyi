package one.xingyi.cddengine
import one.xingyi.cddscenario.Scenario

trait NewTreeAndTraceData[P, R] {
  def data: DecisionTreeFoldingData[P, R]
  def newTree: DecisionTree[P, R]
  def scenario: Scenario[P, R] = data.fd.scenario
  def st = data.st
  def oldNode = data.fd.conclusionNode
}

case class NewTreeAndTraceDataWithIssue[P, R](data: DecisionTreeFoldingData[P, R], newTree: DecisionTree[P, R], newIssue: DecisionIssue[P, R]) extends NewTreeAndTraceData[P, R]
case class NewTreeAndTraceDataWithNewNode[P, R](data: DecisionTreeFoldingData[P, R], newTree: DecisionTree[P, R], newNode: DecisionTreeNode[P, R]) extends NewTreeAndTraceData[P, R]

case class TraceDataWithIndex[P, R](traceData: NewTreeAndTraceData[P, R], index: String)

