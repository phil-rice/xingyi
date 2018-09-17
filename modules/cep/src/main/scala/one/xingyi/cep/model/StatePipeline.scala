/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
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
