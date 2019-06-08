/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
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
