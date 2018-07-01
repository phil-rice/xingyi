package one.xingyi.cep


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
  //  def stateL[ED] = Lens[StoreStateAndPipeline[ED], StoredState[ED]](_.miyamotoState, (big, s) => big.copy(miyamotoState = s))
  def makeIfCan[ED: StringFieldGetter](thisEd: ED)(storedState: StoredState[ED]): Option[PipelineData[ED]] = {
    import storedState._
    storedState.currentState.find(thisEd).map { pipeline =>
      val startData = pipeline.event.makeMap(thisEd).getOrElse(Map())
      PipelineData(key, thisEd, currentState, data + (pipeline.event -> startData), pipeline, pipeline.event, List())
    }
  }
}
