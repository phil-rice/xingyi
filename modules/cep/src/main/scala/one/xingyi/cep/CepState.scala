package one.xingyi.cep
import one.xingyi.core.optics.Lens

object MiyamotoState {
  def lastDataL[ED] = Lens[MiyamotoState[ED], Map[Event, StringMap]](_.data, (m, e) => m.copy(data = e))
  def edL[ED] = Lens[MiyamotoState[ED], ED](_.ed, (m, e) => m.copy(ed = e))
  def stateL[ED] = Lens[MiyamotoState[ED], CepState](_.currentState, (m, e) => m.copy(currentState = e))
}
case class MiyamotoState[ED](key: Any, ed: ED, currentState: CepState = terminate, data: Map[Event, StringMap] = Map())

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

