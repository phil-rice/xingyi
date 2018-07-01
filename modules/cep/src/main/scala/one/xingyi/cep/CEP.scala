package one.xingyi.cep

import one.xingyi.core.builder.{Aggregator, HasAggregator, HasId, RememberingAggregator2}
import one.xingyi.core.language.FunctionLanguage._
import one.xingyi.core.optics.Lens
import one.xingyi.core.reflection.Macros

import scala.collection.concurrent.TrieMap
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.language.experimental.macros

case class ListenerRef(any: Any)

trait StringFieldGetter[ED] {
  def getString(stringField: StringField): ED => Option[String]
}
trait CEP[ED] extends StringFieldGetter[ED] {
  def listenTo(fn: ED => Unit): ListenerRef
  def stopListeningTo(ref: ListenerRef)
  def sendMessage(topicEvent: TopicEvent, emitData: EmitData)
}

object StoredState {
  def lastDataL[ED] = Lens[StoredState[ED], Map[Event, StringMap]](_.data, (m, e) => m.copy(data = e))
  def edL[ED] = Lens[StoredState[ED], ED](_.ed, (m, e) => m.copy(ed = e))
  def stateL[ED] = Lens[StoredState[ED], CepState](_.currentState, (m, e) => m.copy(currentState = e))
}

case class StoredState[ED](key: Any, ed: ED, currentState: CepState = Terminate, data: Map[Event, StringMap] = Map())


class CEPProcessor[ED](topicEvent: TopicEvent, preprocess: Processor)(implicit cep: CEP[ED]) {
  val map: TrieMap[Any, StoredState[ED]] = TrieMap()

  def findLastStateFromString: ED => String => StoredState[ED] = ed => key => map.getOrElseUpdate(key, new StoredState[ED](key, ed, preprocess.initial, Map()))
  def findLastStateFromED = cep.getString(preprocess.keyby) ~+?> findLastStateFromString
  def processPipeline: PipelineData[ED] => PipelineData[ED] = { s => s.statePipeline.execute(s) }
  def processFinalState: PipelineData[ED] => Option[StoredState[ED]] = { s: PipelineData[ED] => s.statePipeline.finalState().processAtEnd(map)(s) }
  def emitMessages = { s: PipelineData[ED] => s.emitData.foreach(cep.sendMessage(topicEvent, _)) }

  def process = findLastStateFromED ~~+?> PipelineData.makeIfCan[ED] ~?> processPipeline ~?^> processFinalState ~?^> emitMessages
}

trait HasKeyBy {def keyby: StringField}



