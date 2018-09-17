/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cep.model

import one.xingyi.cep._

import scala.collection.concurrent.TrieMap


trait CepState {
  def name: String
  def list: List[StatePipeline]
  def findStatePipeline[ED: StringFieldGetter](ed: ED): Option[StatePipeline] = list.find(_.event.accepts(ed))
  def processAtEnd[ED](trieMap: TrieMap[Any, StoredState[ED]])(pipelineData: PipelineData[ED]): Option[StoredState[ED]]
}

object Terminate extends CepState {
  override def list: List[StatePipeline] = List()
  override def name: String = "terminate"
  override def processAtEnd[ED](trieMap: TrieMap[Any, StoredState[ED]])(pipelineData: PipelineData[ED]): Option[StoredState[ED]] = {trieMap.remove(pipelineData.key); None}
}

case class UserState(name: String, var list: List[StatePipeline]) extends CepState {
  override def toString: String = s"State($name, pipelines = ${list.mkString(" || ")})"
  override def processAtEnd[ED](trieMap: TrieMap[Any, StoredState[ED]])(pipelineData: PipelineData[ED]): Option[StoredState[ED]] =
    trieMap.put(pipelineData.key, pipelineData.asStoredStateWithNewState)
}

