package one.xingyi.cep.model

import one.xingyi.cep.{PipelineData, StoredState, StringFieldGetter, StringMap}

import scala.annotation.tailrec

object StatePipeline {
  def startData[ED: StringFieldGetter](ed: ED)(pipeline: StatePipeline): StringMap = pipeline.event.makeMap(ed).getOrElse(Map())
}
case class StatePipeline(event: StartEvent, pipelineStages: List[PipelineStage], finalState: () => CepState) {

  def asStartData[ED: StringFieldGetter](thisEd: ED, s: StoredState[ED]) =
    PipelineData(s.key, thisEd, s.currentState, s.data + (event -> StatePipeline.startData(thisEd)(this)), this, event, List())
  def execute[ED](startState: PipelineData[ED]): PipelineData[ED] = execute(startState, pipelineStages)
  @tailrec
  final private def execute[ED](state: PipelineData[ED], pipelineStages: List[PipelineStage]): PipelineData[ED] = pipelineStages match {
    case Nil => state
    case stage :: tail => execute(stage.execute(state), tail)
  }

  def >>(pipelineStage: PipelineStage) = StatePipeline(event, pipelineStages :+ pipelineStage, finalState)
  def >>(finalState: => CepState) = StatePipeline(event, pipelineStages, () => finalState)

  private def pipelineStageAsString: String = if (pipelineStages.isEmpty) "" else " => " + pipelineStages.mkString(" => ")
  override def toString: String = s"($event$pipelineStageAsString => ${finalState().name})"
}
