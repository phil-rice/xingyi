package one.xingyi.cep.model

import one.xingyi.cep._
import one.xingyi.core.builder.RememberingAggregator2
import one.xingyi.core.language.AnyLanguage._
import one.xingyi.core.misc.PublicIdMaker

import scala.concurrent.duration.Duration
import scala.language.experimental.macros

trait Event {
  def name: String
  def findDataForThisEvent(map: LastEventAndData): Option[StringMap]
}

case class MapEvent(name: String) extends EventWithFields

object NullEvent extends Event {
  override def name: String = "NullEvent"
  override def findDataForThisEvent(map: LastEventAndData): Option[StringMap] = Some(map.dataForLastEvent)
}

trait StartEvent extends Event {
  def makeMap[ED](ed: ED)(implicit stringFieldGetter: StringFieldGetter[ED]): Option[StringMap]
  def accepts[ED: StringFieldGetter](lastEvent: ED): Boolean
  def >>(s: => CepState): StatePipeline = StatePipeline(this, List(), () => s)
  def >>(p: PipelineStage) = StatePipeline(this, List(p), () => Terminate)
}

case class Timeout(n: Duration) extends StartEvent {
  override def findDataForThisEvent(map: LastEventAndData): Option[StringMap] = Some(map.dataForLastEvent)
  override def accepts[ED: StringFieldGetter](lastEvent: ED): Boolean = false
  override def makeMap[ED](ed: ED)(implicit stringFieldGetter: StringFieldGetter[ED]): Option[StringMap] = None
  override def name: String = "timeout"
}

trait EventWithFields extends Event with  PublicIdMaker {
  //TODO This  is dripping with anti pattern. Put simply needing this sucks. I think we can do this better with macros, but this works for now


  protected implicit val currentValues = new InheritableThreadLocal[LastEventAndData]
  implicit def implicitsValuesUntilWeGetMacrosSortedOut = currentValues.get

  protected implicit val aggregator: RememberingAggregator2[StringField, String] = new RememberingAggregator2()
  def fields = aggregator.items

  def stringField: StringField = macro Macros.stringFieldImpl

  def makeMap[ED](ed: ED)(implicit stringFieldGetter: StringFieldGetter[ED]) =
    fields.foldLeftWithOptionsEatingExceptions[StringMap](Map()) { (acc, sf) => sf.get(ed).map(v => acc + (sf.name -> v)) }

  def findDataForThisEvent(lastEventAndData: LastEventAndData) = {
    currentValues.set(lastEventAndData)
    fields.foldLeftWithOptionsEatingExceptions[StringMap](Map()) { (acc, sf) => Some(acc + (sf.name -> sf.value)) }
  }
}

//TODO Again just until we get the macros sorted out...this is a mess at the moment to make the DSL prettier
abstract class TopicEvent(val name: String, val topic: Topic, val version: String = "1.0.0") extends StartEvent with EventWithFields {
  override def accepts[ED: StringFieldGetter](ed: ED): Boolean = makeMap(ed).exists { m => currentValues.set(new LastEventAndDataForAccept(this, m)); actualWhere(m) }
  var actualWhere: WhereFn = { _ => true }
  def where(fn: => Boolean) = actualWhere = _ => fn
  override def toString: String = s"TopicEvent($name,$version)"
}
