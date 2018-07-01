package one.xingyi.cep

trait PipelineStage {
  def execute[ED](state: PipelineData[ED]): PipelineData[ED]
}

trait CepAction extends PipelineStage

case class EmitData(map: StringMap)

object Emit extends CepAction {
  override def execute[ED](state: PipelineData[ED]): PipelineData[ED] = state.copy(emitData = state.emitData :+ EmitData(state.dataForLastEvent))
  override def toString: String = "emit"
}
object purge extends CepAction {
  override def execute[ED](state: PipelineData[ED]): PipelineData[ED] = state
  override def toString: String = "purge"
}

class CouldntCreateMapEventException(state: PipelineData[_], event: MapEvent) extends RuntimeException(
  s"""
     |State is $state
     |MapEvent $event
   """.stripMargin)

case class map(event: MapEvent) extends CepAction {
  override def execute[ED](state: PipelineData[ED]): PipelineData[ED] = {
    event.update(state) match {
      case Some(newMap) => state.copy(lastEvent = event, data = state.data + (event -> newMap))
      case _ => throw new CouldntCreateMapEventException(state, event)
    }
  }
}
