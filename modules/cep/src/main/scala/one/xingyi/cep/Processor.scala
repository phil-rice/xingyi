package one.xingyi.cep
import one.xingyi.core.builder.{Aggregator, HasAggregator, HasId}
import one.xingyi.core.reflection.Macros
import scala.language.experimental.macros


case class Topic(topicName: String, version: String)

abstract class Processor(name: String, version: String) extends HasKeyBy {
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

