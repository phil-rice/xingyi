package one.xingyi.cep

import one.xingyi.core.builder.RememberingAggregator2
import one.xingyi.core.misc.{IdMaker, PublicIdMaker}
import one.xingyi.core.reflection.Macros

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.language.experimental.macros

case class MapEvent() extends Event with WithFields

trait Event {
  def accepts(lastEvent: Any): Boolean = ???
  def update(map: Map[StringField, String]): Map[StringField, String]
  def >>(s: => CepState): StatePipeline = StatePipeline(this, List(), () => s)
  def >>(p: PipelineStage) = StatePipeline(this, List(p), () => terminate)
}

trait WithFields extends PublicIdMaker {
  //TODO This is an anti pattern. Put simply needing this sucks. I think we can do this better with macros, but this works for now
  protected implicit def currentValues = new InheritableThreadLocal[StringMap] {
    override def initialValue(): StringMap = Map()
  }
  implicit def implicitsValuesUntilWeGetMacrosSortedOut = currentValues.get

  protected implicit val aggregator: RememberingAggregator2[StringField] = new RememberingAggregator2[StringField]()
  def stringField: StringField = macro Macros.stringFieldImpl

  def fields = aggregator.items
  def update(map: StringMap) = {currentValues.set(map); fields.foldLeft[StringMap](Map())((acc, sf) => acc + (sf -> sf.value(map)))}
}


abstract class TopicEvent(name: String, topic: Topic, version: String = "1.0.0") extends Event {
  //TODO Again just until we get the macros sorted out
  var actualWhere: WhereFn = { _ => true }
  def where(fn: => Boolean) = actualWhere = _ => fn
  override def toString: String = s"TopicEvent($name,$version)"
}

case class timeout(n: Duration) extends Event {
  override def update(map: Map[StringField, String]): Map[StringField, String] = map
}

case class StatePipelineAndMiyamotoState[ED](statePipeline: StatePipeline, miyamotoState: MiyamotoState[ED])
object StatePipelineAndMiyamotoState {
  def apply[ED](miyamotoState: MiyamotoState[ED])(statePipeline: StatePipeline): StatePipelineAndMiyamotoState[ED] = StatePipelineAndMiyamotoState(statePipeline, miyamotoState)
}
case class StatePipeline(event: Event, pipelineStages: List[PipelineStage], finalState: () => CepState) {
  def execute[ED](startState: MiyamotoState[ED]): MiyamotoState[ED] = execute(startState, pipelineStages).copy(currentState = finalState())

  @tailrec
  final private def execute[ED](state: MiyamotoState[ED], pipelineStages: List[PipelineStage]): MiyamotoState[ED] = pipelineStages match {
    case Nil => state
    case stage :: tail => execute(stage.execute(state), tail)
  }

  def >>(pipelineStage: PipelineStage) = StatePipeline(event, pipelineStages :+ pipelineStage, finalState)
  def >>(finalState: => CepState) = StatePipeline(event, pipelineStages, () => finalState)
  def pipelineStageAsString= if (pipelineStages.isEmpty) "" else " => " + pipelineStages.mkString(" => ")
  override def toString: String = s"($event$pipelineStageAsString => ${finalState().name})"
}




