/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddengine

import java.io.{File, PrintWriter}

import one.xingyi.cddscenario.{HasEngineComponentData, Scenario}
import one.xingyi.core.json.JsonLanguage._
import one.xingyi.core.json.{JsonList, JsonObject, JsonValue}
import one.xingyi.core.language.AnyLanguage._
import one.xingyi.core.reflection.IsDefinedInSourceCodeAt
import one.xingyi.core.strings.{Files, ShortPrint, Strings}
import one.xingyi.core.language.FunctionLanguage._

trait UrlGenerator[T] extends (T => String) {
  def trace(prefix: String, t: T)(implicit renderConfig: RenderingConfig) = Strings.uri(renderConfig.rootTraceDirectory, prefix, apply(t))
  def print(prefix: String, t: T)(implicit renderConfig: RenderingConfig) = Strings.uri(renderConfig.rootPrintDirectory, prefix, apply(t))
}

object RenderingConfig {
  implicit val default = RenderingConfig()
}
case class RenderingConfig(rootTraceDirectory: String = "target/cdd/trace", rootPrintDirectory: String = "target/cdd/print")

object DecisionTreeRendering {
  def simple[P: ShortPrint, R: ShortPrint](implicit urlGenerator: EngineUrlGenerators[P, R]): DecisionTreeRendering[JsonObject, P, R] = new SimpleDecisionTreeRendering[P, R]
  def withScenario[P: ShortPrint, R: ShortPrint](data: WithScenarioData[P, R])(implicit urlGenerator: EngineUrlGenerators[P, R]): DecisionTreeRendering[JsonObject, P, R] = new WithScenarioRendering[P, R](data)
  //  def trace = new TraceRenderer
  //  def print = new PrintPagesRenderer
}

object PrintRenderToFile {
  implicit val default = new PrintRenderToFile
}
class PrintRenderToFile {
  def apply(file: String)(pwFn: PrintWriter => Unit) = Files.printToFile(new File(file))(pwFn)
}

class TraceRenderer {
  def apply[P, R](rendering: WithScenarioData[P, R] => DecisionTreeRendering[String, P, R], prefix: String)(engine: Engine[P, R])(implicit validation: Validation[P, R], finder: DtFolderStrategyFinder, renderingConfig: RenderingConfig, printRenderToFile: PrintRenderToFile, urlGenerators: EngineUrlGenerators[P, R]) = {
    val scenarios = engine.tools.scenarios
    val list: Seq[NewTreeAndTraceData[P, R]] = DecisionTreeTracing.trace[P, R](scenarios).reverse
    val indexs = list.zipWithIndex.collect {
      case (n: NewTreeAndTraceDataWithNewNode[P, R], i) => s"<a href=${urlGenerators.scenario(n.scenario)}>${n.scenario.logic.definedInSourceCodeAt} ${n.st.getClass.getSimpleName} ${n.scenario.situation}</a>"
      case (n: NewTreeAndTraceDataWithIssue[P, R], i) => s"<a href=${urlGenerators.scenario(n.scenario)}>${n.scenario.logic.definedInSourceCodeAt} ${n.newIssue.getClass.getSimpleName} ${n.scenario.situation}</a>"
    }
    val indexPage = indexs.mkString("<br />\n")
    list.zipWithIndex.foreach { case t@(traceData, i) => printRenderToFile(urlGenerators.scenario.trace(prefix, traceData.scenario)) { pw =>
      val theseScenarios = scenarios.take(i + 1)
      val actualRendering = rendering(WithScenarioData(traceData.scenario, DecisionTree.findWithParentsForScenario(traceData.newTree)(traceData.scenario)))
      pw.write(actualRendering.traceDataWithIndex apply TraceDataWithIndex(traceData, indexPage))
    }
    }
    printRenderToFile(urlGenerators.engine.trace(prefix, engine))(_.print(indexPage))
  }
}

class PrintPagesRenderer {

  def apply[P, R](rendering: DecisionTreeRendering[String, P, R], scenarioRendering: WithScenarioData[P, R] => DecisionTreeRendering[String, P, R])(prefix: String, engine: Engine[P, R])(implicit renderingConfig: RenderingConfig, printRenderToFile: PrintRenderToFile, urlGenerators: EngineUrlGenerators[P, R]) = {
    printRenderToFile(urlGenerators.engine.print(prefix, engine))(_.print(rendering.engine(engine)))
    engine.tools.useCases.zipWithIndex.foreach { case (uc, i) => printRenderToFile(urlGenerators.usecase.print(prefix, uc))(pw => pw.print(rendering.useCase(uc))) }
    engine.tools.scenarios.zipWithIndex.foreach { case (s, i) =>
      val actualRendering = scenarioRendering(WithScenarioData(s, DecisionTree.findWithParentsForScenario(engine.tools.decisionTree)(s)))
      printRenderToFile(urlGenerators.scenario.print(prefix, s))(pw => pw.print(actualRendering.engine(engine)))
    }
  }

}

//We could have broken this down to individual type classes, but the type signatures would be horrible as we need to be able to pass in custom renderers and there is one per entity
trait DecisionTreeRendering[J, P, R] {
  def traceDataWithIndex: TraceDataWithIndex[P, R] => J
  def traceData: NewTreeAndTraceData[P, R] => J
  def engine: Engine[P, R] => J
  def tree: DecisionTree[P, R] => J
  def useCase: UseCase[P, R] => J
  def scenario: Scenario[P, R] => J
  def node: DecisionTreeNode[P, R] => J = {case c: ConclusionNode[P, R] => conclusionNode(c); case d: DecisionNode[P, R] => decisionNode(d)}
  def decisionNode: DecisionNode[P, R] => J
  def conclusionNode: ConclusionNode[P, R] => J
  def issue: DecisionIssue[P, R] => J
  def andThen[J1](fn: J => J1): DecisionTreeRendering[J1, P, R] = new TransformingTreeRending(this, fn)
}

class TransformingTreeRending[J, J1, P, R](val rendering: DecisionTreeRendering[J, P, R], val transform: J => J1) extends DecisionTreeRendering[J1, P, R] {
  override def traceDataWithIndex: TraceDataWithIndex[P, R] => J1 = rendering.traceDataWithIndex andThen transform
  override def traceData: NewTreeAndTraceData[P, R] => J1 = rendering.traceData andThen transform
  override def engine: Engine[P, R] => J1 = rendering.engine andThen transform
  override def tree: DecisionTree[P, R] => J1 = rendering.tree andThen { p => println("in println" + p); p } andThen transform
  override def useCase(): UseCase[P, R] => J1 = rendering.useCase andThen transform
  override def scenario: Scenario[P, R] => J1 = rendering.scenario andThen transform
  override def decisionNode: DecisionNode[P, R] => J1 = rendering.decisionNode andThen transform
  override def conclusionNode: ConclusionNode[P, R] => J1 = rendering.conclusionNode andThen transform
  override def issue: DecisionIssue[P, R] => J1 = rendering.issue andThen transform
}
class SimpleDecisionTreeRendering[P, R](implicit shortPrintP: ShortPrint[P], shortPrintR: ShortPrint[R], urlGenerator: EngineUrlGenerators[P, R]) extends DecisionTreeRendering[JsonObject, P, R] {
  implicit class JsonObjectOps(j: JsonObject) {
    def addOpt(nameAndValues: (String, Option[String])*) = JsonObject((j.nameAndValues ++ nameAndValues.collect { case (name, Some(value)) => Seq(name -> (value: JsonValue)) }.flatten): _*)
    def data[T](t: T)(implicit getData: HasEngineComponentData[T]): JsonObject = using(getData(t)) { data => j |+| ("defined" -> data.definedInSourceCodeAt.toString) addOpt("title" -> data.title, "comment" -> data.comment) }
    def defined[T](t: T)(implicit isDefinedInSourceCodeAt: IsDefinedInSourceCodeAt[T]) = j |+| ("defined" -> isDefinedInSourceCodeAt(t).toString)
    def addOptList(nameAndList: (String, List[JsonObject])) = if (nameAndList._2.size > 0) j |+| (nameAndList._1 -> JsonList(nameAndList._2), ("has" + nameAndList._1) -> true) else j

  }
  override def traceDataWithIndex: TraceDataWithIndex[P, R] => JsonObject = td => JsonObject("traceData" -> traceData(td.traceData), "index" -> td.index)
  override def traceData: NewTreeAndTraceData[P, R] => JsonObject = {
    t =>
      val raw = JsonObject(
        "tree" -> tree(t.newTree),
        "scenario" -> scenario(t.scenario),
        "oldNode" -> conclusionNode(t.oldNode),
      )
      t match {
        case n: NewTreeAndTraceDataWithIssue[P, R] => raw |+| ("issue" -> issue(n.newIssue))
        case n: NewTreeAndTraceDataWithNewNode[P, R] => raw |+| ("newNode" -> node(n.newNode))
      }
  }
  override def engine = e => JsonObject("useCases" -> JsonList(e.tools.useCases.map(useCase)), "tree" -> tree(e.tools.decisionTree), "url" -> urlGenerator.engine(e))
  override def tree = tree => JsonObject("root" -> node(tree.root)) addOptList ("issues" -> tree.issues.map(issue))
  override def useCase = u => JsonObject("useCase" -> JsonObject("scenarioTitle" -> true, "scenarios" -> JsonList(u.allScenarios.map(scenario)), "url" -> urlGenerator.usecase(u)).data(u))
  override def decisionNode: DecisionNode[P, R] => JsonObject = d => JsonObject("decisionNode" -> JsonObject("condition" -> toJsonString(d.logic.ifString), "ifTrue" -> node(d.ifTrue), "ifFalse" -> node(d.ifFalse))).defined(d)
  override def conclusionNode: ConclusionNode[P, R] => JsonObject = c => JsonObject("conclusionNode" -> JsonObject("scenarios" -> JsonList(c.scenarios.map(scenario)))).defined(c)
  override def scenario: Scenario[P, R] => JsonObject = s => JsonObject("situation" -> shortPrintP(s.situation), "url" -> urlGenerator.scenario(s)).data(s)
  override def issue: DecisionIssue[P, R] => JsonObject = e => JsonObject("issue" -> e.toString)
}

case class WithScenarioData[P, R](s: Scenario[P, R], nodes: List[DecisionTreeNode[P, R]])

sealed abstract class NodeEffect(val json: String)
case object GoesThrough extends NodeEffect("goes_through")
case object WouldGoThrough extends NodeEffect("would_go_through")
case object Fails extends NodeEffect("fails")
case object NotApplicable extends NodeEffect("not_on_path")
object NodeEffect {
  type NodeEffectNodeFn[P, R] = WithScenarioData[P, R] => DecisionTreeNode[P, R] => Option[NodeEffect]

  def goesThroughNode[P, R]: NodeEffectNodeFn[P, R] = withScenarioData => node => withScenarioData.nodes.find(_ == node).map(_ => GoesThrough)
  def wouldGoThroughNode[P, R]: NodeEffectNodeFn[P, R] = withScenarioData => node => node.logic.accept(withScenarioData.s).toOption(WouldGoThrough)
  def failsDecisionNode[P, R]: NodeEffectNodeFn[P, R] = withScenarioData => {
    case node: DecisionNode[P, R] => Some(Fails);
    case _ => None
  }
  def apply[P, R](withScenarioData: WithScenarioData[P, R]): DecisionTreeNode[P, R] => NodeEffect = goesThroughNode[P, R] orElse wouldGoThroughNode orElse failsDecisionNode orDefault NotApplicable apply withScenarioData
}

class WithScenarioRendering[P, R](withScenarioData: WithScenarioData[P, R])(implicit urlGenerator: EngineUrlGenerators[P, R]) extends SimpleDecisionTreeRendering[P, R] {
  implicit class WithSituationJsonObjectOps[From <: DecisionTreeNode[P, R]](j: From => JsonObject) {
    def addNodeEffect: From => JsonObject = { from => j(from) |+| ("node" -> NodeEffect(withScenarioData)(from).json) }
  }
  override def decisionNode: DecisionNode[P, R] => JsonObject = super.decisionNode.addNodeEffect
  override def conclusionNode: ConclusionNode[P, R] => JsonObject = super.conclusionNode.addNodeEffect
  override def useCase: UseCase[P, R] => JsonObject = uc => if (uc.allScenarios.contains(withScenarioData.s)) super.useCase(uc) else JsonObject()
  override def scenario: Scenario[P, R] => JsonObject = s => if (withScenarioData.s == s) super.scenario(s) |+| "selected" -> "selected" else super.scenario(s)
}

