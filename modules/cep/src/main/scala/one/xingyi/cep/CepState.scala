package one.xingyi.cep

case class MiyamotoState[ED](key: Any, lastEvent: ED, currentState: CepState = terminate, lastData: Map[Event, Map[StringField, String]] = Map())

trait CepState {
  def name: String
  def list: List[StatePipeline]
}

object terminate extends CepState {
  override def list: List[StatePipeline] = List()
  override def name: String = "terminate"
}

case class UserState(name: String, var list: List[StatePipeline]) extends CepState {
   override def toString: String = s"State($name, pipelines = ${list.mkString(" || ")})"
}

