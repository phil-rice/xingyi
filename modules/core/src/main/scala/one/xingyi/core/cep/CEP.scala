package one.xingyi.core.cep

package com.ing.miyamoto.two
import com.ing.miyamoto.two.MiyamotoDsl2.{StringField, keyby}

import scala.concurrent.duration.Duration
import scala.concurrent.duration._


trait HasKeyBy {
  def keyby: StringField

}
case class Topic(topicName: String, version: String)

trait PipelineStage {
  //  def >>(s: SpecialThing): TopicEvent = ???
  //  def >>(s: State): TopicEvent = ???

}

trait Action extends PipelineStage
object emit extends Action
object purge extends Action

object terminate extends State

case class MapEvent() extends Event


trait Event {
  def >>(s: State): StatePipeline = StatePipeline(this, List(), s)
  def >>(p: PipelineStage): StatePipeline = StatePipeline(this, List(p), terminate)
}
object Event {

}

abstract class TopicEvent(topic: Topic, version: String) extends Event with HasKeyBy {
  protected def fields(block: => Unit): Unit = {
    block
  }
  //  def >>(s: SpecialThing): Event = ???
}
case class timeout(n: Duration) extends Event

case class StatePipeline(event: Event, pipelineStages: List[PipelineStage], finalState: State) {
  def >>(pipelineStage: PipelineStage) = StatePipeline(event, pipelineStages :+ pipelineStage, finalState)
  def >>(finalState: State) = StatePipeline(event, pipelineStages, finalState)
}


abstract class TopicEventWithWhere(topic: Topic, version: String = "") extends TopicEvent(topic, version) {
  def where: Boolean
}

case class Timeout(duration: Duration) extends Event


abstract class preprocess(name: String) {
  def initial: State
  case class version(v: String) {
    def using(block: => Unit): Unit = {

    }
  }
}


object MiyamotoDsl2 extends MiyamotoDsl2
trait MiyamotoDsl2 {

  def map(fn: => MapEvent): PipelineStage = ???


  def string(fieldName: String) = {

  }


  implicit class StringMiyamotoOps(s: String) {
    def :=(s: String) = {

    }
  }


  class StringField {
    def value: String = ???
    def withvalue(string: String): StringField = ???
  }
  def stringField: StringField = ???


  case class publish(name: TopicEvent) {
    case class version(v: String) {
      def using(block: => Unit) = ???

    }
  }

}

class MiyamotoState[ED] {
  var lastData = Map[TopicEvent, ED]()
  var lastEvent: ED = ???
}

trait State
object State {
  def apply(fn: StatePipeline): UserState = new UserState(List(fn))
}


case class UserState(list: List[StatePipeline]) extends State {
  def or(pipeline: StatePipeline): State = UserState(list :+ pipeline)
}


trait Sample2 extends MiyamotoDsl2 {
  val fraudtestbusinesseventstopic = Topic("fraudtestbusinesseventstopic", "1.0.0")
  val fraudtestinputtopic = Topic("fraudtestinputtopic", "1.0.0")

  trait CustomerAddressIpAddressAndType {
    val `type` = stringField
    val customerId = stringField
    val ipaddress = stringField
    def keyby = customerId
  }

  val be2 = new TopicEvent(fraudtestbusinesseventstopic, "1.0.0") with CustomerAddressIpAddressAndType
  // Preprocess Definition for the BE1 event"

  val pp2 = new preprocess("1.0.0") {
    val ie1 = new TopicEventWithWhere(fraudtestinputtopic) with CustomerAddressIpAddressAndType {
      def where = `type`.value == "A"
    }
    val ie2 = new TopicEventWithWhere(fraudtestinputtopic) with CustomerAddressIpAddressAndType {
      def where = `type`.value == "B" && customerId.value.matches("[A02468]$")
    }

    val ie3 = new TopicEventWithWhere(fraudtestinputtopic) with CustomerAddressIpAddressAndType {
      def where = `type`.value == "F" && customerId.value.matches("[A02468]$")
    }

    val map123 = new MapEvent {
      val ipaddress = stringField withvalue (ie1.ipaddress.value + ie2.ipaddress.value + ie3.ipaddress.value)
      val `type` = stringField withvalue (ie1.`type`.value + "-" + ie2.`type`.value + "-" + ie3.`type`.value)
      val businessEventSubtype = stringField withvalue "performance-test-data"
    }

    def initial = State {ie1 >> ie1Recv}
    def ie1Recv = State {timeout(11 seconds) >> purge >> terminate} or {ie2 >> ie2Recv}
    def ie2Recv = State {ie3 >> map(map123) >> emit >> terminate}
  }

  case class bind(e: preprocess) {
    def to(topic: TopicEvent) = ???
  }
  bind(pp2) to (be2)
}