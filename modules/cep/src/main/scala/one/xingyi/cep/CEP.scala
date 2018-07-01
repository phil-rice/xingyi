package one.xingyi.cep

import one.xingyi.cep.model._
import one.xingyi.core.language.FunctionLanguage._

import scala.collection.concurrent.TrieMap
import scala.language.experimental.macros
import scala.language.postfixOps


trait StringFieldGetter[ED] extends (StringField => ED => Option[String])

object StringFieldGetter {implicit def stringGetterForCep[ED](implicit cEP: CEP[ED]): StringFieldGetter[ED] = cEP.getString}

trait CEP[ED] {
  def getString: StringFieldGetter[ED]
  def sendMessage(topicEvent: TopicEvent, emitData: EmitData)
}

case class StoredState[ED](key: Any, ed: ED, currentState: CepState = Terminate, data: Map[Event, StringMap] = Map())

trait HasKeyBy {def keyby: StringField}

class CEPProcessor[ED](topicEvent: TopicEvent, preprocess: Processor)(implicit cep: CEP[ED]) {
  val map: TrieMap[Any, StoredState[ED]] = TrieMap()
  def findLastStateFromString: ED => String => StoredState[ED] = ed => key => map.getOrElseUpdate(key, new StoredState[ED](key, ed, preprocess.initial, Map()))
  def findLastStateFromED: ED => Option[StoredState[ED]] = cep.getString(preprocess.keyby) ~+?> findLastStateFromString
  def processPipeline: PipelineData[ED] => PipelineData[ED] = { s => s.statePipeline.execute(s) }
  def processFinalState: PipelineData[ED] => Option[StoredState[ED]] = { s: PipelineData[ED] => s.statePipeline.finalState().processAtEnd(map)(s) }
  def emitMessages: PipelineData[ED] => Unit = { s: PipelineData[ED] => s.emitData.foreach(cep.sendMessage(topicEvent, _)) }

  def process: ED => Option[PipelineData[ED]] = findLastStateFromED ~~+?> PipelineData.makeIfCan[ED] ~?> processPipeline ~?^> processFinalState ~?^> emitMessages
}




