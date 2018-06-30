package one.xingyi.cep

trait PipelineStage {
  def execute[ED](state: MiyamotoState[ED]): MiyamotoState[ED]
}

trait CepAction extends PipelineStage
object emit extends CepAction {
  override def execute[ED](state: MiyamotoState[ED]): MiyamotoState[ED] = state
  override def toString: String = "emit"
}
object purge extends CepAction {
  override def execute[ED](state: MiyamotoState[ED]): MiyamotoState[ED] = state
  override def toString: String = "purge"
}
case class map(event: MapEvent) extends CepAction {
  override def execute[ED](state: MiyamotoState[ED]): MiyamotoState[ED] = ???
}
