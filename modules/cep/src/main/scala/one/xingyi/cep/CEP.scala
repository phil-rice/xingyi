package one.xingyi.cep

import one.xingyi.core.builder.{Aggregator, HasAggregator, HasId}
import one.xingyi.core.language.FunctionLanguage._
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
}


class CEPProcessor[ED](topic: Topic, preprocess: Preprocess)(implicit cep: CEP[ED]) {
  val map: TrieMap[Any, MiyamotoState[ED]] = TrieMap()
  val toDataL = StatePipelineAndMiyamotoState.stateL[ED] andThen MiyamotoState.lastDataL
  def findLastStateFromString: ED => String => MiyamotoState[ED] = ed => key => map.getOrElseUpdate(key, new MiyamotoState[ED](key, ed, preprocess.initial, Map()))
  def findLastStateFromED = cep.getString(preprocess.keyby) ~+?> findLastStateFromString
  def updateStateWithEd = MiyamotoState.edL[ED].setFn
  def findPipeline: MiyamotoState[ED] => Option[StatePipelineAndMiyamotoState[ED]] = { state => state.currentState.find(state.ed).map {StatePipelineAndMiyamotoState.apply(state)} }

  def updateWithStartState: StatePipelineAndMiyamotoState[ED] => StatePipelineAndMiyamotoState[ED] = { s => toDataL.transform(s, _ + (s.statePipeline.event -> s.makeMapForEventFromED)) }

  def processPipeline: StatePipelineAndMiyamotoState[ED] => MiyamotoState[ED] = {case StatePipelineAndMiyamotoState(statePipeline, state) => statePipeline.execute(state); state.copy(currentState = statePipeline.finalState())}
  def putBackInMap: MiyamotoState[ED] => Unit = { s => map.put(s.key, s) }

  def process = findLastStateFromED ~+?> updateStateWithEd ~~?> findPipeline ~?> updateWithStartState ~?> processPipeline ~?> putBackInMap
}

trait HasKeyBy {
  def keyby: StringField
}

object StringField {
  implicit object hasIdForStringField extends HasId[StringField, Int] {override def apply(v1: StringField): Int = v1.id}
}

abstract class StringField(implicit val aggregator: Aggregator[StringField]) extends HasAggregator[StringField] {
  def id: Int
  def name: String
  aggregator(this)
  def :=(fn: ValueFn): StringField = new StringFieldWithValue(id, name, fn)
  def :=(value: String): StringField = new StringFieldWithValue(id, name, _ => value)
  def :==(value: String): StringField = macro Macros.assignmentImpl
  def value(implicit map: StringMap) = map(name)
  override def toString: String = s"StringField($id,$name)"
}

case class KeyByStringField(name: String) extends StringField()(Aggregator.nullAggregator[StringField]) {
  override def id: Int = -1
}
case class SimpleStringField(id: Int, name: String)(implicit aggregator: Aggregator[StringField]) extends StringField {
  override def toString: String = s"StringField($id,$name)"
}

case class StringFieldWithValue(id: Int, name: String, value: ValueFn)(implicit aggregator: Aggregator[StringField]) extends StringField


case class Topic(topicName: String, version: String)


abstract class Preprocess(name: String, version: String) extends HasKeyBy {
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
  override def toString: String = s"Preprocess($name,$version:\n   ${setupList.map { t => t._1 }.mkString("\n   ")}"
}



