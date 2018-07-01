package one.xingyi.cep

import one.xingyi.core.builder.RememberingAggregator2
import one.xingyi.core.language.AnyLanguage._
import one.xingyi.core.misc.PublicIdMaker
import one.xingyi.core.reflection.Macros

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.language.experimental.macros

case class MapEvent(name: String) extends Event with WithFields {
  override def event: Event = this
}

trait Event {
  def name: String
  def update(map: LastEventAndData): Option[StringMap]
}

object NullEvent extends Event {
  override def name: String = "NullEvent"
  override def update(map: LastEventAndData): Option[StringMap] = Some(map.dataForLastEvent)
}

trait StartEvent extends Event {
  def makeMap[ED](ed: ED)(implicit stringFieldGetter: StringFieldGetter[ED]): Option[StringMap]
  def accepts[ED: StringFieldGetter](lastEvent: ED): Boolean
  def >>(s: => CepState): StatePipeline = StatePipeline(this, List(), () => s)
  def >>(p: PipelineStage) = StatePipeline(this, List(p), () => Terminate)
}

trait WithFields extends PublicIdMaker {
  //TODO This is another anti pattern. Sort it out.
  def event: Event

  //TODO This is an anti pattern. Put simply needing this sucks. I think we can do this better with macros, but this works for now
  protected implicit val currentValues = new InheritableThreadLocal[LastEventAndData]
  implicit def implicitsValuesUntilWeGetMacrosSortedOut = currentValues.get

  protected implicit val aggregator: RememberingAggregator2[StringField, String] = new RememberingAggregator2()
  def fields = aggregator.items

  def stringField: StringField = macro Macros.stringFieldImpl

  def makeMap[ED](ed: ED)(implicit stringFieldGetter: StringFieldGetter[ED]) =
    fields.foldLeftWithOptionsEatingExceptions[StringMap](Map()) { (acc, sf) => sf.get(ed).map(v => acc + (sf.name -> v)) }

  import one.xingyi.core.map.Maps._
  def update(lastEventAndData: LastEventAndData) = {
    currentValues.set(lastEventAndData) //TODO another place that needs the 'value' macro sorting out
    fields.foldLeftWithOptionsEatingExceptions[StringMap](Map()) { (acc, sf) => Some(acc + (sf.name -> sf.value)) }
  }
}


abstract class TopicEvent(val name: String, val topic: Topic, val version: String = "1.0.0") extends StartEvent with WithFields {
  def event = this
  override def accepts[ED](ed: ED)(implicit cep: StringFieldGetter[ED]): Boolean = makeMap(ed).map { m => currentValues.set(new LastEventAndDataForAccept(this, m)); actualWhere(m) }.getOrElse(false)
  //TODO Again just until we get the macros sorted out...this is a mess at the moment to make the DSL prettier
  var actualWhere: WhereFn = { _ => true }
  def where(fn: => Boolean) = actualWhere = _ => fn
  override def toString: String = s"TopicEvent($name,$version)"
}

case class Timeout(n: Duration) extends StartEvent {
  override def update(map: LastEventAndData): Option[StringMap] = Some(map.dataForLastEvent)
  override def accepts[ED: StringFieldGetter](lastEvent: ED): Boolean = false
  override def makeMap[ED](ed: ED)(implicit stringFieldGetter: StringFieldGetter[ED]): Option[StringMap] = None
  override def name: String = "timeout"
}

case class StatePipeline(event: StartEvent, pipelineStages: List[PipelineStage], finalState: () => CepState) {
  def execute[ED](startState: PipelineData[ED]): PipelineData[ED] = execute(startState, pipelineStages)

  @tailrec
  final private def execute[ED](state: PipelineData[ED], pipelineStages: List[PipelineStage]): PipelineData[ED] = pipelineStages match {
    case Nil => state
    case stage :: tail => execute(stage.execute(state), tail)
  }

  def >>(pipelineStage: PipelineStage) = StatePipeline(event, pipelineStages :+ pipelineStage, finalState)
  def >>(finalState: => CepState) = StatePipeline(event, pipelineStages, () => finalState)
  def pipelineStageAsString = if (pipelineStages.isEmpty) "" else " => " + pipelineStages.mkString(" => ")
  override def toString: String = s"($event$pipelineStageAsString => ${finalState().name})"
}




