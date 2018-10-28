package one.xingyi.cddengine
import one.xingyi.cddscenario.Scenario
import one.xingyi.core.optics.Lens

object DtFolderStrategyFinder {
  implicit val defaultFinder: DtFolderStrategyFinder = new SimpleDtFolderStrategyFinder
}
//Holds more data than needed to allow debugging and tracing
case class DataNeededToMakeANewTree[P, R](treeAndScenario: TreeAndScenario[P, R], st: DTFolderStrategy) {
  def oldTree = treeAndScenario.tree
  def conclusionAndScenario = treeAndScenario.conclusionAndScenario
  def oldNode = treeAndScenario.conclusionAndScenario.conclusionNode
  def lens = treeAndScenario.lens
  def scenario: Scenario[P, R] = treeAndScenario.scenario
}

trait DtFolderStrategyFinder {
  def apply[P, R](conclusionAndScenario: ConclusionAndScenario[P, R]): DTFolderStrategy
  def findStrategyAndApply[P, R]: TreeAndScenario[P, R] => DataNeededToMakeANewTree[P, R] =
    treeAndScenario => DataNeededToMakeANewTree(treeAndScenario, apply(treeAndScenario.conclusionAndScenario))

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
