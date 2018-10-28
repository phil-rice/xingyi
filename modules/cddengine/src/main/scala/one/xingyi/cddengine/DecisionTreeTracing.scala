package one.xingyi.cddengine
import one.xingyi.cddscenario.Scenario

object DecisionTreeTracing {
  private def treeFrom[P, R](list: List[NewTreeAndTraceData[P, R]]) = list.headOption.fold(DecisionTree.empty[P, R])(_.newTree)
  def trace[P, R](list: Seq[Scenario[P, R]])(implicit strategy: DtFolderStrategyFinder, decisionTreeFoldingDataToNewTreeAndTraceData: DecisionTreeFoldingDataToNewTreeAndTraceData): List[NewTreeAndTraceData[P, R]] =
    list.zipWithIndex.foldLeft[List[NewTreeAndTraceData[P, R]]](List()) { case (acc, (s, i)) => decisionTreeFoldingDataToNewTreeAndTraceData(strategy.findStrategyAndApply(treeFrom(acc), s)) :: acc }

}
