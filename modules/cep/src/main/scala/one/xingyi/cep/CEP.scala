/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cep

import one.xingyi.cep.model._
import one.xingyi.core.language.FunctionLanguage._

import scala.collection.concurrent.TrieMap
import scala.language.experimental.macros
import scala.language.postfixOps


trait StringFieldGetter[ED] extends (StringField => ED => Option[String])

object StringFieldGetter {implicit def stringGetterForCep[ED](implicit cEP: CEP[ED]): StringFieldGetter[ED] = cEP.getString}

trait CEP[ED] {
  def getString: StringFieldGetter[ED]
  def sendMessage(topicEvent: TopicEvent, emitData: EmitData)
}

case class StoredState[ED](key: Any, ed: ED, currentState: CepState = Terminate, data: Map[Event, StringMap] = Map())

trait HasKeyBy {def keyby: StringField}

class CEPProcessor[ED](topicEvent: TopicEvent, preprocess: Processor)(implicit cep: CEP[ED]) {
  val map: TrieMap[Any, StoredState[ED]] = TrieMap()
  def findLastStateFromString: ED => String => StoredState[ED] = ed => key => map.getOrElseUpdate(key, new StoredState[ED](key, ed, preprocess.initial, Map()))
  def findLastStateFromED: ED => Option[StoredState[ED]] = cep.getString(preprocess.keyby) ~+?> findLastStateFromString
  def processPipeline: PipelineData[ED] => PipelineData[ED] = { s => s.statePipeline.execute(s) }
  def processFinalState: PipelineData[ED] => Option[StoredState[ED]] = { s: PipelineData[ED] => s.statePipeline.finalState().processAtEnd(map)(s) }
  def emitMessages: PipelineData[ED] => Unit = { s: PipelineData[ED] => s.emitData.foreach(cep.sendMessage(topicEvent, _)) }

  def process: ED => Option[PipelineData[ED]] = findLastStateFromED ~~+?> PipelineData.makeIfCan[ED] ~?> processPipeline ~?^> processFinalState ~?^> emitMessages
}




