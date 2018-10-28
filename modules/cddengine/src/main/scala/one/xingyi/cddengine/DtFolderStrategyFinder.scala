package one.xingyi.cddengine
import one.xingyi.cddscenario.Scenario
import one.xingyi.core.optics.Lens

object DtFolderStrategyFinder {
  implicit val defaultFinder: DtFolderStrategyFinder = new SimpleDtFolderStrategyFinder
}
case class DecisionTreeFoldingData[P, R](oldTree: DecisionTree[P, R], st: DTFolderStrategy, lens: Lens[DecisionTreeNode[P, R], DecisionTreeNode[P, R]], fd: ConclusionAndScenario[P, R])

trait DtFolderStrategyFinder {
  def apply[P, R](conclusionAndScenario: ConclusionAndScenario[P, R]): DTFolderStrategy
  def findStrategyAndApply[P, R](tree: DecisionTree[P, R], s: Scenario[P, R]): DecisionTreeFoldingData[P, R] = {
    val (lens, lensCn) = tree.root.findLensAndCnLens(s.situation)
    val node: ConclusionNode[P, R] = lensCn(tree.root)
    val fd = ConclusionAndScenario(node, s)
    DecisionTreeFoldingData(tree, apply(fd), lens, fd)
  }
}

class SimpleDtFolderStrategyFinder extends DtFolderStrategyFinder {
  val folders = List[DTFolderStrategy](
    AddScenarioToEmptyConclusion,
    AddScenarioReplaceLogic,
    AddScenarioMergeCondition,
    AddScenarioToConclusion,
    MakeDecisionNodeScenarioAsFalse,
    MakeDecisionNodeScenarioAsTrue,
    ScenariosClash)

  def apply[P, R](fd: ConclusionAndScenario[P, R]): DTFolderStrategy = folders.find(_.isDefinedAt(fd)).getOrElse(throw new RuntimeException(s"Cannot work out how to deal with $fd"))
}

