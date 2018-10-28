/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddengine

import java.text.MessageFormat

import one.xingyi.cddengine
import one.xingyi.cddscenario.{HasScenarios, Scenario, ScenarioLogic}
import one.xingyi.core.json.{JsonMaps, JsonObject, JsonWriter, TemplateEngine}
import one.xingyi.core.misc.IdMaker
import one.xingyi.core.optics.Lens

import scala.collection.concurrent.TrieMap
import scala.language.higherKinds


object JsonDataForTree {
  def make[J: JsonWriter, P, R](data: WithScenarioData[P, R])(jsonObject: JsonObject): JsonDataForTree[J, P, R] = JsonDataForTree[J, P, R](jsonObject, Some(data))
}
case class JsonDataForTree[J: JsonWriter, P, R](jsonObject: JsonObject, data: Option[WithScenarioData[P, R]]) extends JsonMaps[J](jsonObject)

trait Engine[P, R] extends PartialFunction[P, R] {
  def tools: EngineTools[P, R]
}

trait EngineTools[P, R] {
  def useCases: Seq[UseCase[P, R]]
  def scenarios: Seq[Scenario[P, R]]
  def decisionTree: DecisionTree[P, R]
  def issues: Seq[DecisionIssue[P, R]]
  def printTraceAboutAdding[J: JsonWriter](prefix: String)(implicit config: RenderingConfig, validation: Validation[P, R], template: TemplateEngine[J], urlGenerators: EngineUrlGenerators[P, R], printRenderToFile: PrintRenderToFile): Unit
  def printPages[J: JsonWriter](prefix: String)(implicit config: RenderingConfig, template: TemplateEngine[J], urlGenerators: EngineUrlGenerators[P, R], printRenderToFile: PrintRenderToFile): Unit
  def test(name: String): CddTest
  def trace(p: P): TraceThroughEngineResultData[P, R]
}

case class TraceThroughEngineResultData[P, R](trace: List[DecisionTreeNode[P, R]], lens: Lens[DecisionTreeNode[P, R], DecisionTreeNode[P, R]])

class SimpleEngineTools[P, R](engine: Engine1[P, R]) extends EngineTools[P, R] {
  override def decisionTree: DecisionTree[P, R] = engine.decisionTree
  override def scenarios: Seq[Scenario[P, R]] = engine.scenarios
  override def useCases: Seq[UseCase[P, R]] = engine.useCases
  protected def printPrinter[J: JsonWriter](implicit engineUrlGenerators: EngineUrlGenerators[P, R], config: RenderingConfig, template: TemplateEngine[J]): DecisionTreeRendering[String, P, R] =
    DecisionTreeRendering.simple[P, R] andThen (x => JsonDataForTree[J, P, R](x, None)) andThen template.apply
  protected def tracePrinter[J: JsonWriter](data: WithScenarioData[P, R])(implicit engineUrlGenerators: EngineUrlGenerators[P, R], template: TemplateEngine[J]): DecisionTreeRendering[String, P, R] =
    new WithScenarioRendering[P, R](data) andThen JsonDataForTree.make[J, P, R](data) andThen template.apply
  override def printTraceAboutAdding[J: JsonWriter](prefix: String)(implicit renderingConfig: RenderingConfig, validation: Validation[P, R], template: TemplateEngine[J], urlGenerators: EngineUrlGenerators[P, R], printRenderToFile: PrintRenderToFile): Unit =
    (new TraceRenderer).apply(tracePrinter[J], prefix)(engine)

  override def printPages[J: JsonWriter](prefix: String)(implicit renderingConfig: RenderingConfig, template: TemplateEngine[J], urlGenerators: EngineUrlGenerators[P, R], printRenderToFile: PrintRenderToFile): Unit =
    (new PrintPagesRenderer).apply[P, R](printPrinter[J], tracePrinter[J])(prefix, engine)
  def test(name: String): CddTest = new SimpleTestMaker[P, R](name, engine).apply
  override def issues: List[DecisionIssue[P, R]] = engine.decisionTree.issues
  override def trace(p: P): TraceThroughEngineResultData[P, R] = TraceThroughEngineResultData[P, R](DecisionTree.findWithParents(engine.decisionTree, p), decisionTree.root.findLens(p))
}


case class Engine1[P, R](decisionTree: DecisionTree[P, R], scenarios: Seq[Scenario[P, R]], useCases: Seq[UseCase[P, R]]) extends Engine[P, R] {
  def logicFor(p: P): ScenarioLogic[P, R] = decisionTree.root.findLens(p).get(decisionTree.root).logic
  override def isDefinedAt(p: P): Boolean = logicFor(p).fn.isDefinedAt(p)
  override def apply(p: P): R = logicFor(p).fn apply p //later we can be more efficient. Don't optimise just yet
  override val tools: EngineTools[P, R] = new SimpleEngineTools(this)
}


case class BuildEngineRawData[P, R](tree: DecisionTree[P, R], useCases: Seq[UseCase[P, R]], scenarios: Seq[Scenario[P, R]])

trait DecisionTreeMaker[T[_, _], P, R] extends (T[P, R] => BuildEngineRawData[P, R])

object DecisionTreeMaker {
  implicit def defaultDecisionTreeMaker[T[_, _], P, R](implicit hasScenarios: HasScenarios[T], hasUseCases: HasUseCases[T], dtFolder: DecisionTreeFolder): DecisionTreeMaker[T, P, R] = {
    t =>
      val scenarios = hasScenarios.allScenarios[P, R](t)
      val decisionTree = scenarios.foldLeft(DecisionTree.empty[P, R])(dtFolder.apply)
      cddengine.BuildEngineRawData(decisionTree, hasUseCases.useCases[P, R](t), scenarios)
  }
}
object Engine {
  def apply[T[_, _] : HasScenarios : HasUseCases, P, R](t: T[P, R])(implicit decisionTreeMaker: DecisionTreeMaker[T, P, R]): Engine[P, R] = {
    val rawData = decisionTreeMaker(t)
    import rawData._
    Engine1[P, R](tree, scenarios, useCases)
  }
  //  def apply[P, R](title: String)(block: ScenarioAggregator[P, R] => Unit): Engine[P, R] = Engine1(UseCase(title)(block).allScenarios)

}

object EngineUrlGenerators {
  implicit def urlGenerator[P, R] = new SimpleUrlGenerator[P, R]
}
trait EngineUrlGenerators[P, R] {
  def engine: UrlGenerator[Engine[P, R]]
  def usecase: UrlGenerator[UseCase[P, R]]
  def scenario: UrlGenerator[Scenario[P, R]]
}


class SimpleUrlGenerator[P, R](pattern: String = "{0}_{1}.html") extends EngineUrlGenerators[P, R] with IdMaker {
  val map = new TrieMap[Any, Int]()
  def getOrUpdate(a: Any): String = map.getOrElseUpdate(a, getNextId).toString
  def url[T](typeName: String)(t: T) = MessageFormat.format(pattern, typeName, getOrUpdate(t))
  def engineurl[T](typeName: String)(t: T) = MessageFormat.format(pattern, typeName, "index")
  override def engine: UrlGenerator[Engine[P, R]] = engineurl("engine")
  override def usecase: UrlGenerator[UseCase[P, R]] = url("usecase")
  override def scenario: UrlGenerator[Scenario[P, R]] = url("scenario")
}
