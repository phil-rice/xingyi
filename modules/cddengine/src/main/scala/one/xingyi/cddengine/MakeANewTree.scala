package one.xingyi.cddengine


trait MakeANewTree {
  def apply[P, R](treeFoldingData: DataNeededToMakeANewTree[P, R]): NewTreeAndTraceData[P, R]
}
object MakeANewTree {
  implicit def defaultDecisionTreeFoldingDataToNewTreeAndTraceData[P, R]: MakeANewTree = new SimpleMakeANewTree
}

class SimpleMakeANewTree extends MakeANewTree {
  override def apply[P, R](data: DataNeededToMakeANewTree[P, R]): NewTreeAndTraceData[P, R] = {
    import data._
    st(data.conclusionAndScenario) match {
      case Left(issue) => NewTreeAndTraceDataWithIssue(data, oldTree.copy(issues = issue :: oldTree.issues), issue)
      case Right(node) => NewTreeAndTraceDataWithNewNode(data, oldTree.copy(root = lens.set(oldTree.root, node)), node)
    }
  }
}
