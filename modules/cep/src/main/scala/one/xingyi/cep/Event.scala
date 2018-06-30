package one.xingyi.cep

import one.xingyi.core.builder.RememberingAggregator2
import one.xingyi.core.misc.{IdMaker, PublicIdMaker}
import one.xingyi.core.reflection.Macros

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.language.experimental.macros
import scala.util.Try

case class MapEvent() extends Event with WithFields

trait Event {
  def update(map: Map[StringField, String]): Option[Map[StringField, String]]
}

trait StartEvent extends Event {
  def accepts[ED: StringFieldGetter](lastEvent: ED): Boolean
  def >>(s: => CepState): StatePipeline = StatePipeline(this, List(), () => s)
  def >>(p: PipelineStage) = StatePipeline(this, List(p), () => terminate)
}

trait WithFields extends PublicIdMaker {
  //TODO This is an anti pattern. Put simply needing this sucks. I think we can do this better with macros, but this works for now
  protected implicit val currentValues = new InheritableThreadLocal[StringMap] {
    override def initialValue(): StringMap = Map()
  }
  implicit def implicitsValuesUntilWeGetMacrosSortedOut = currentValues.get

  protected implicit val aggregator: RememberingAggregator2[StringField] = new RememberingAggregator2[StringField]()
  def stringField: StringField = macro Macros.stringFieldImpl

  def fields = aggregator.items
  def makeMap[ED](ed: ED)(implicit stringFieldGetter: StringFieldGetter[ED]) = fields.foldLeft[Option[StringMap]](Some(Map())) {
    case (Some(acc), sf) => try {stringFieldGetter.getString(sf)(ed).map(v => acc + (sf -> v))} catch {case e: Exception => None};
    case _ => None
  }

  def update(map: StringMap) = {
    currentValues.set(map) //TODO another place that needs the 'value' macro sorting out
    fields.foldLeft[Option[StringMap]](Some(Map())) { case (Some(acc), sf) => try {Some(acc + (sf -> sf.value(map)))} catch {case e: Exception => None}; case _ => None }
  }
}


abstract class TopicEvent(val name: String, val topic: Topic, val version: String = "1.0.0") extends StartEvent with WithFields {
  override def accepts[ED](lastEvent: ED)(implicit cep: StringFieldGetter[ED]): Boolean = makeMap(lastEvent).map { m => currentValues.set(m); actualWhere(m) }.getOrElse(false)
  //TODO Again just until we get the macros sorted out...this is a mess at the moment to make the DSL prettier
  var actualWhere: WhereFn = { _ => true }
  def where(fn: => Boolean) = actualWhere = _ => fn
  override def toString: String = s"TopicEvent($name,$version)"
}

case class timeout(n: Duration) extends StartEvent {
  override def update(map: Map[StringField, String]): Option[Map[StringField, String]] = Some(map)
  override def accepts[ED: StringFieldGetter](lastEvent: ED): Boolean = true
}

case class StatePipelineAndMiyamotoState[ED](statePipeline: StatePipeline, miyamotoState: MiyamotoState[ED])
object StatePipelineAndMiyamotoState {
  def apply[ED](miyamotoState: MiyamotoState[ED])(statePipeline: StatePipeline): StatePipelineAndMiyamotoState[ED] = StatePipelineAndMiyamotoState(statePipeline, miyamotoState)
}
case class StatePipeline(event: StartEvent, pipelineStages: List[PipelineStage], finalState: () => CepState) {
  def execute[ED](startState: MiyamotoState[ED]): MiyamotoState[ED] = execute(startState, pipelineStages).copy(currentState = finalState())

  @tailrec
  final private def execute[ED](state: MiyamotoState[ED], pipelineStages: List[PipelineStage]): MiyamotoState[ED] = pipelineStages match {
    case Nil => state
    case stage :: tail => execute(stage.execute(state), tail)
  }

  def >>(pipelineStage: PipelineStage) = StatePipeline(event, pipelineStages :+ pipelineStage, finalState)
  def >>(finalState: => CepState) = StatePipeline(event, pipelineStages, () => finalState)
  def pipelineStageAsString = if (pipelineStages.isEmpty) "" else " => " + pipelineStages.mkString(" => ")
  override def toString: String = s"($event$pipelineStageAsString => ${finalState().name})"
}




