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
