/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddengine
import one.xingyi.cddscenario.Scenario

object DecisionTreeBuildTracing {
  private def treeFrom[P, R](list: List[NewTreeAndTraceData[P, R]]) = list.headOption.fold(DecisionTree.empty[P, R])(_.newTree)
  def trace[P, R](list: Seq[Scenario[P, R]])(implicit strategy: DtFolderStrategyFinder, decisionTreeFoldingDataToNewTreeAndTraceData: MakeANewTree): List[NewTreeAndTraceData[P, R]] =
    list.zipWithIndex.foldLeft[List[NewTreeAndTraceData[P, R]]](List()) { case (acc, (s, i)) => decisionTreeFoldingDataToNewTreeAndTraceData(strategy.findStrategyAndApply(TreeAndScenario(treeFrom(acc), s))) :: acc }

}
