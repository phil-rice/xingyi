package one.xingyi.cddengine

trait DecisionTreeFoldingDataToNewTreeAndTraceData {
  def apply[P, R](treeFoldingData: DecisionTreeFoldingData[P, R]): NewTreeAndTraceData[P, R]
}
object DecisionTreeFoldingDataToNewTreeAndTraceData {
  implicit def defaultDecisionTreeFoldingDataToNewTreeAndTraceData[P, R]: DecisionTreeFoldingDataToNewTreeAndTraceData = new SimpleDecisionTreeFoldingDataToNewTreeAndTraceData
}


class SimpleDecisionTreeFoldingDataToNewTreeAndTraceData extends DecisionTreeFoldingDataToNewTreeAndTraceData {
  override def apply[P, R](treeFoldingData: DecisionTreeFoldingData[P, R]): NewTreeAndTraceData[P, R] = {
    import treeFoldingData._
    st(fd) match {
      case Left(issue) => NewTreeAndTraceDataWithIssue(treeFoldingData, oldTree.copy(issues = issue :: oldTree.issues), issue)
      case Right(node) => NewTreeAndTraceDataWithNewNode(treeFoldingData, oldTree.copy(root = lens.set(oldTree.root, node)), node)
    }
  }
}
