package one.xingyi.cep
import one.xingyi.cep.model.{CepState, Macros, StatePipeline, UserState}
import one.xingyi.core.builder.{Aggregator, HasAggregator, HasId}

import scala.language.experimental.macros


case class Topic(topicName: String, version: String)

abstract class Processor(name: String, version: String) extends HasKeyBy {
  def initial: CepState

  protected implicit class PipeLineOps(p: StatePipeline) {
    def or(p2: StatePipeline) = List(p, p2)
    def ||(p2: StatePipeline) = List(p, p2)
  }
  protected implicit class PipeLineListOps(p: List[StatePipeline]) {
    def or(p2: StatePipeline) = p :+ p2
    def ||(p2: StatePipeline) = p :+ p2
  }

  def newState(state: UserState, block: => List[StatePipeline]) = {
    setupList = setupList :+ (state, block)
    state
  }

  protected var setupList: List[(UserState, List[StatePipeline])] = List()
  //TODO replace this with a build operation that will return a side effect free model.  Don't bother until the DSL is better defined
  def initialise = setupList.foreach { case (state, list) => state.list = list }
  protected def state(block: StatePipeline): UserState = macro Macros.statePipelineImpl
  protected def state(block: List[StatePipeline]): UserState = macro Macros.statePipelinesImpl
  override def toString: String = s"Preprocess($name,$version:\n   ${setupList.map { t => t._1 }.mkString("\n   ")}"
}

