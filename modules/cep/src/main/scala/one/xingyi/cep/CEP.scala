package one.xingyi.cep

import one.xingyi.core.builder.{Aggregator, HasAggregator, HasId, RememberingAggregator2}
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
  def sendMessage(topicEvent: TopicEvent, emitData: EmitData)
}


class CEPProcessor[ED](topicEvent: TopicEvent, preprocess: Preprocess)(implicit cep: CEP[ED]) {
  val map: TrieMap[Any, StoredState[ED]] = TrieMap()

  def findLastStateFromString: ED => String => StoredState[ED] = ed => key => map.getOrElseUpdate(key, new StoredState[ED](key, ed, preprocess.initial, Map()))
  def findLastStateFromED = cep.getString(preprocess.keyby) ~+?> findLastStateFromString
  def processPipeline: PipelineData[ED] => PipelineData[ED] = { s => s.statePipeline.execute(s) }
  def putResultInMap: PipelineData[ED] => Option[StoredState[ED]] = { s: PipelineData[ED] =>
    s.statePipeline.finalState() match {
      case Terminate => map.remove(s.key)
      case _ => map.put(s.key, s.asStoredStateWithNewState)
    }
  }
  def emitMessages = { s: PipelineData[ED] => s.emitData.foreach(cep.sendMessage(topicEvent, _)) }

  def process = findLastStateFromED ~~+?> PipelineData.makeStartIfCan[ED] ~?> processPipeline ~?^> putResultInMap ~?^> emitMessages
}

trait HasKeyBy {
  def keyby: StringField
}

object StringField {
  implicit object hasIdForStringField extends HasId[StringField, String] {override def apply(v1: StringField): String = v1.name}
}

class CannotgetData(stringField: StringField, lastEventAndData: LastEventAndData, name: String, event: Event, cause: Throwable) extends RuntimeException(
  s"""
     |stringField: $stringField
     |name:  $name
     |event: $event
     |Data:
     |$lastEventAndData
  """.stripMargin, cause)

abstract class StringField(implicit val aggregator: Aggregator[StringField]) extends HasAggregator[StringField] {
  def aggregatorString = aggregator match {
    case a: RememberingAggregator2[StringField, _] => aggregator.asInstanceOf[RememberingAggregator2[StringField, _]].items.mkString("\n  ")
    case x => x.toString()
  }
  aggregator(this)
  //  println(s"making ${getClass.getSimpleName}. event: $event name: $name,  Aggregator is \n  $aggregatorString")


  def name: String
  def event: Event
//  def :=(fn: ValueFn): StringField = new StringFieldWithValue(event, name, fn)
//  def :=(value: String): StringField = new StringFieldWithValue(event, name, _ => value)
  def :=(value: String): StringField = macro Macros.assignmentImpl
  def value(implicit lastEventAndData: LastEventAndData) = try {lastEventAndData.data(event)(name)} catch {case e: Exception => throw new CannotgetData(this, lastEventAndData, name, event, e)}
  override def toString: String = s"StringField( $event,  $name)"
}

case class KeyByStringField(name: String) extends StringField()(Aggregator.nullAggregator[StringField]) {
  def event = NullEvent
}
case class SimpleStringField(event: Event, name: String)(implicit aggregator: Aggregator[StringField]) extends StringField
case class StringFieldWithValue(event: Event, name: String, valueFn: ValueFn)(implicit aggregator: Aggregator[StringField]) extends StringField {
  override def value(implicit lastEventAndData: LastEventAndData) = valueFn(lastEventAndData.data)
  override def toString: String = s"StringFieldWithValue( $event, $name)"
}


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



