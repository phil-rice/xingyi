package one.xingyi.cep
import one.xingyi.core.optics.Lens

object StoredState {
  def lastDataL[ED] = Lens[StoredState[ED], Map[Event, StringMap]](_.data, (m, e) => m.copy(data = e))
  def edL[ED] = Lens[StoredState[ED], ED](_.ed, (m, e) => m.copy(ed = e))
  def stateL[ED] = Lens[StoredState[ED], CepState](_.currentState, (m, e) => m.copy(currentState = e))
}

case class StoredState[ED](key: Any, ed: ED, currentState: CepState = terminate, data: Map[Event, StringMap] = Map())

case class StoreStateAndPipeline[ED](key: Any, ed: ED, currentState: CepState, data: Map[Event, StringMap], statePipeline: StatePipeline) {
  def makeMapForEventFromED(implicit stringFieldGetter: StringFieldGetter[ED]): StringMap = statePipeline.event.makeMap(ed).getOrElse(Map())
}
case class PipelineData[ED](key: Any, ed: ED, currentState: CepState, data: Map[Event, StringMap], statePipeline: StatePipeline, lastEvent: Event) {
  def asStoredStateWithNewState = StoredState(key, ed, statePipeline.finalState(), data)

}

object PipelineData {
  //  def stateL[ED] = Lens[StoreStateAndPipeline[ED], StoredState[ED]](_.miyamotoState, (big, s) => big.copy(miyamotoState = s))
  def makeStartIfCan[ED: StringFieldGetter](thisEd: ED)(storedState: StoredState[ED]): Option[PipelineData[ED]] = {
    import storedState._
    storedState.currentState.find(thisEd).map { pipeline =>
      val startData = pipeline.event.makeMap(thisEd).getOrElse(Map())
      PipelineData(key, thisEd, currentState, data + (pipeline.event -> startData), pipeline, pipeline.event)
    }
  }
}


trait CepState {
  def name: String
  def list: List[StatePipeline]
  def find[ED: StringFieldGetter](ed: ED) = list.find(_.event.accepts(ed))
}

object terminate extends CepState {
  override def list: List[StatePipeline] = List()
  override def name: String = "terminate"
}

case class UserState(name: String, var list: List[StatePipeline]) extends CepState {
  override def toString: String = s"State($name, pipelines = ${list.mkString(" || ")})"
}

