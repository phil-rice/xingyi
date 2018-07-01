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

