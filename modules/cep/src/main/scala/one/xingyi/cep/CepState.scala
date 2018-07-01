package one.xingyi.cep
import one.xingyi.core.optics.Lens

object StoredState {
  def lastDataL[ED] = Lens[StoredState[ED], Map[Event, StringMap]](_.data, (m, e) => m.copy(data = e))
  def edL[ED] = Lens[StoredState[ED], ED](_.ed, (m, e) => m.copy(ed = e))
  def stateL[ED] = Lens[StoredState[ED], CepState](_.currentState, (m, e) => m.copy(currentState = e))
}

case class StoredState[ED](key: Any, ed: ED, currentState: CepState = Terminate, data: Map[Event, StringMap] = Map())

case class StoreStateAndPipeline[ED](key: Any, ed: ED, currentState: CepState, data: Map[Event, StringMap], statePipeline: StatePipeline) {
  def makeMapForEventFromED(implicit stringFieldGetter: StringFieldGetter[ED]): StringMap = statePipeline.event.makeMap(ed).getOrElse(Map())
}

trait LastEventAndData {
  def lastEvent: Event
  def data: Map[Event, StringMap]
  def map = data(lastEvent)
  def getOrException(name: String) = data(lastEvent)(name)
}
case class LastEventAndDataForAccept(lastEvent: Event, rawMap: StringMap) extends LastEventAndData {
  val data = Map(lastEvent -> rawMap)
  override def toString: String = s"LastEventAndDataForAccept($lastEvent -> $map)"
}
case class LastEventAndDataForTest(lastEvent: Event, data: Map[Event, StringMap]) extends LastEventAndData

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
  def makeStartIfCan[ED: StringFieldGetter](thisEd: ED)(storedState: StoredState[ED]): Option[PipelineData[ED]] = {
    import storedState._
    storedState.currentState.find(thisEd).map { pipeline =>
      val startData = pipeline.event.makeMap(thisEd).getOrElse(Map())
      PipelineData(key, thisEd, currentState, data + (pipeline.event -> startData), pipeline, pipeline.event, List())
    }
  }
}


trait CepState {
  def name: String
  def list: List[StatePipeline]
  def find[ED: StringFieldGetter](ed: ED) = list.find(_.event.accepts(ed))
}

object Terminate extends CepState {
  override def list: List[StatePipeline] = List()
  override def name: String = "terminate"
}

case class UserState(name: String, var list: List[StatePipeline]) extends CepState {
  override def toString: String = s"State($name, pipelines = ${list.mkString(" || ")})"
}

