/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cep
import one.xingyi.cep.model.{CepState, EmitData, Event, StatePipeline}


trait LastEventAndData {
  def lastEvent: Event
  def data: Map[Event, StringMap]
  def dataForLastEvent = data(lastEvent)
  def getOrException(name: String) = data(lastEvent)(name)
}

case class LastEventAndDataForAccept(lastEvent: Event, rawMap: StringMap) extends LastEventAndData {val data = Map(lastEvent -> rawMap)}

case class PipelineData[ED](key: Any, ed: ED, startState: CepState, data: Map[Event, StringMap], statePipeline: StatePipeline, lastEvent: Event, emitData: List[EmitData]) extends LastEventAndData {
  def asStoredStateWithNewState = StoredState(key, ed, statePipeline.finalState(), data)
  override def toString: String =
    s""""PipelineData($key,$ed
       |startState: $startState
       |Data
       |  ${data.mkString("\n  ")}
       |Pipeline: $statePipeline
       |Event $lastEvent
     """.stripMargin

}
object PipelineData {
  //TODO this can be made clearer.
  def makeIfCan[ED: StringFieldGetter](thisEd: ED)(s: StoredState[ED]): Option[PipelineData[ED]] =
    s.currentState.findStatePipeline(thisEd).map(_.asStartData(thisEd, s))

}
