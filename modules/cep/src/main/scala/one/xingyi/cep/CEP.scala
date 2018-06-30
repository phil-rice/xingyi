package one.xingyi.cep

import one.xingyi.core.builder.{Aggregator, HasAggregator, HasId}
import one.xingyi.core.language.FunctionLanguage._
import one.xingyi.core.reflection.Macros

import scala.collection.concurrent.TrieMap
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.language.experimental.macros

case class ListenerRef(any: Any)

trait CEP[ED] {
  def listenTo(fn: ED => Unit): ListenerRef
  def stopListeningTo(ref: ListenerRef)
  def getString(stringField: StringField): ED => String
}


class CEPProcessor[ED](keyby: StringField, topic: Topic, preprocess: Preprocess)(implicit cep: CEP[ED]) {
  val map: TrieMap[Any, MiyamotoState[ED]] = TrieMap()

  def findLastStateFromString: ED => String => MiyamotoState[ED] = ed => key => map.getOrElseUpdate(key, new MiyamotoState[ED](key, ed, preprocess.initial, Map()))
  def findLastStateFromED = cep.getString(keyby) ~+> findLastStateFromString
  def updateStateWithEd = { ed: ED => state: MiyamotoState[ED] => state.copy(lastEvent = ed) }
  def findPipeline: MiyamotoState[ED] => Option[StatePipelineAndMiyamotoState[ED]] = { state =>
    state.currentState.list.find(_.event.accepts(state.lastEvent)).map {StatePipelineAndMiyamotoState.apply(state)}
  }
  def updateWithStartState: StatePipelineAndMiyamotoState[ED] => StatePipelineAndMiyamotoState[ED] = {
    case StatePipelineAndMiyamotoState(statePipeline, state) =>
      val mapForState = state.lastData.getOrElse(statePipeline.event, Map())
      val newMapForState = statePipeline.event.update(mapForState)
      StatePipelineAndMiyamotoState(statePipeline, state.copy(lastData = state.lastData + (statePipeline.event -> newMapForState)))
  }
  def processPipeline: StatePipelineAndMiyamotoState[ED] => MiyamotoState[ED] = {case StatePipelineAndMiyamotoState(statePipeline, state) => statePipeline.execute(state)}
  def putBackInMap: MiyamotoState[ED] => Unit = { s => map.put(s.key, s) }

  def process = findLastStateFromED ~+> updateStateWithEd ~> findPipeline ~?> updateWithStartState ~?> processPipeline ~?> putBackInMap
}

trait HasKeyBy {
  def keyby: StringField
}

object StringField {
  implicit object hasIdForStringField extends HasId[StringField, Int] {override def apply(v1: StringField): Int = v1.id}
}
class StringField(val id: Int, val name: String, val aggregator: Aggregator[StringField]) extends HasAggregator[StringField] {
  aggregator(this)
  def :=(fn: ValueFn): StringField = new StringFieldWithValue(id, name, aggregator)(fn)
  def :=(value: String): StringField = new StringFieldWithValue(id, name, aggregator)(_ => value)
  def :==(value: String): StringField = macro Macros.assignmentImpl
  def value(implicit map: Map[StringField, String]) = map(this)
}

class StringFieldWithValue(id: Int, name: String, aggregator: Aggregator[StringField])(value: ValueFn) extends StringField(id, name, aggregator)


case class Topic(topicName: String, version: String)


abstract class Preprocess(name: String, version: String) {
  protected implicit class PipeLineOps(p: StatePipeline) {
    def or(p2: StatePipeline) = List(p, p2)
    def ||(p2: StatePipeline) = List(p, p2)
  }
  protected implicit class PipeLineListOps(p: List[StatePipeline]) {
    def or(p2: StatePipeline) = p :+ p2
    def ||(p2: StatePipeline) = p :+ p2
  }
  def initial: CepState

  def newState(state: UserState, block: => List[StatePipeline]) = {
    setupList = setupList :+ (state, block)
    state
  }
  protected var setupList: List[(UserState, List[StatePipeline])] = List()
  def initialise = setupList.foreach { case (state, list) => state.list = list }
  protected def state(block: StatePipeline): UserState = macro Macros.statePipelineImpl
  protected def state(block: List[StatePipeline]): UserState = macro Macros.statePipelinesImpl
  override def toString: String = s"Preprocess($name,$version:\n   ${setupList.map{t => t._1}.mkString("\n   ")}"
}



