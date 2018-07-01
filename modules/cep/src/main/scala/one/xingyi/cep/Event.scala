package one.xingyi.cep

import one.xingyi.core.builder.RememberingAggregator2
import one.xingyi.core.misc.{IdMaker, PublicIdMaker}
import one.xingyi.core.optics.Lens
import one.xingyi.core.reflection.Macros

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.language.experimental.macros
import scala.util.Try

case class MapEvent(name: String) extends Event with WithFields {
  override def event: Event = this
}

trait Event {
  def name: String
  def update(map: StringMap): Option[StringMap]
}

object NullEvent extends Event{
  override def name: String = "NullEvent"
  override def update(map: StringMap): Option[StringMap] = Some(map)
}

trait StartEvent extends Event {
  def makeMap[ED](ed: ED)(implicit stringFieldGetter: StringFieldGetter[ED]): Option[StringMap]
  def accepts[ED: StringFieldGetter](lastEvent: ED): Boolean
  def >>(s: => CepState): StatePipeline = StatePipeline(this, List(), () => s)
  def >>(p: PipelineStage) = StatePipeline(this, List(p), () => terminate)
}

trait WithFields extends PublicIdMaker {
  def event: Event
  //TODO This is an anti pattern. Put simply needing this sucks. I think we can do this better with macros, but this works for now
  protected implicit val currentValues = new InheritableThreadLocal[StringMap] {
    override def initialValue(): StringMap = Map()
  }
  implicit def implicitsValuesUntilWeGetMacrosSortedOut = currentValues.get

  protected implicit val aggregator: RememberingAggregator2[StringField] = new RememberingAggregator2[StringField]()
  def stringField: StringField = macro Macros.stringFieldImpl

  def fields = aggregator.items
  def makeMap[ED](ed: ED)(implicit stringFieldGetter: StringFieldGetter[ED]) = fields.foldLeft[Option[StringMap]](Some(Map())) {
    case (Some(acc), sf) => try {stringFieldGetter.getString(sf)(ed).map(v => acc + (sf.name -> v))} catch {case e: Exception => None};
    case _ => None
  }

  def update(map: StringMap) = {
    currentValues.set(map) //TODO another place that needs the 'value' macro sorting out
    fields.foldLeft[Option[StringMap]](Some(Map())) { case (Some(acc), sf) => try {Some(acc + (sf.name -> sf.value(map)))} catch {case e: Exception => None}; case _ => None }
  }
}


abstract class TopicEvent(val name: String, val topic: Topic, val version: String = "1.0.0") extends StartEvent with WithFields {
  def event = this
  override def accepts[ED](lastEvent: ED)(implicit cep: StringFieldGetter[ED]): Boolean = makeMap(lastEvent).map { m => currentValues.set(m); actualWhere(m) }.getOrElse(false)
  //TODO Again just until we get the macros sorted out...this is a mess at the moment to make the DSL prettier
  var actualWhere: WhereFn = { _ => true }
  def where(fn: => Boolean) = actualWhere = _ => fn
  override def toString: String = s"TopicEvent($name,$version)"
}

case class timeout(n: Duration) extends StartEvent {
  override def update(map: StringMap): Option[StringMap] = Some(map)
  override def accepts[ED: StringFieldGetter](lastEvent: ED): Boolean = false
  override def makeMap[ED](ed: ED)(implicit stringFieldGetter: StringFieldGetter[ED]): Option[StringMap] = None
  override def name: String = "timeout"
}

case class StatePipeline(event: StartEvent, pipelineStages: List[PipelineStage], finalState: () => CepState) {
  def execute[ED](startState: PipelineData[ED]): PipelineData[ED] = execute(startState, pipelineStages).copy(currentState = finalState())

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




