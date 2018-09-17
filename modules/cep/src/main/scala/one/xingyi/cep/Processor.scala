/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
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

