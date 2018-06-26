/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddengine

import one.xingyi.cddscenario._
import one.xingyi.core.reflection.DefinedInSourceCodeAt

import scala.language.higherKinds

trait HasUseCases[T[_, _]] {
  def useCases[P, R](t: T[P, R]): List[UseCase[P, R]]

}

object UseCase {
  implicit def ucHasComponentData[P, R]: HasEngineComponentData[UseCase[P, R]] = { u => u.data }

}
trait UseCase[P, R] {
  def data: EngineComponentData
  def allScenarios: List[Scenario[P, R]]
  def allUseCases: List[UseCase[P, R]]
  def copyWithOnly(list: List[Scenario[P, R]]): UseCase[P, R] = {SimpleUseCase(data, allScenarios.filter(list.contains))}
}

case class SimpleUseCase[P, R](data: EngineComponentData, allScenarios: List[Scenario[P, R]]) extends UseCase[P, R] {
  override def allUseCases: List[UseCase[P, R]] = List()
}
object UseCase1 {
  implicit def hasScenarios: HasScenarios[UseCase1] = new HasScenarios[UseCase1] {
    override def allScenarios[P, R](t: UseCase1[P, R]): List[Scenario[P, R]] = t.allScenarios
  }
  implicit def usHasUseCases: HasUseCases[UseCase1] = new HasUseCases[UseCase1] {
    override def useCases[P, R](t: UseCase1[P, R]): List[UseCase1[P, R]] = List(t)
  }
}
class UseCase1[P, R](val data: EngineComponentData) extends UseCase[P, R] with EngineBuilderLanguage1 {
  def this(title: String) = this(EngineComponentData(definedInSourceCodeAt = DefinedInSourceCodeAt.definedInSourceCodeAt(), title = Some(title)))
  protected implicit val aggregator = new RememberingScenarioAggregator2[P, R]
  def allScenarios = aggregator.scenarios
  def or(useCase1: UseCase1[P, R]) = new CompositeUseCase[P, R](List(this, useCase1), EngineComponentData(DefinedInSourceCodeAt.definedInSourceCodeAt(), None))
  override def allUseCases: List[UseCase[P, R]] = List()
}

class UseCase2[P1, P2, R](data: EngineComponentData) extends UseCase1[(P1, P2), R](data) with EngineBuilderLanguage2 {
  def this(title: String) = this(EngineComponentData(definedInSourceCodeAt = DefinedInSourceCodeAt.definedInSourceCodeAt(), title = Some(title)))
  //  protected def scenario(p1: P1, p2: P2): RawSituation2[P1, P2] = {
  //    val data = ScenarioBuilderData[(P1, P2), Nothing](getNextId, (p1, p2), title = None, isDefinedAt = DefinedInSourceCodeAt.definedInSourceCodeAt(2))
  //    RawSituation2(p1, p2, data)
  //  }
}

object CompositeUseCase {
  implicit def hasScenarios = new HasScenarios[CompositeUseCase] {
    override def allScenarios[P, R](t: CompositeUseCase[P, R]): List[Scenario[P, R]] = t.allScenarios
  }
  implicit def usHasUseCases: HasUseCases[CompositeUseCase] = new HasUseCases[CompositeUseCase] {
    override def useCases[P, R](t: CompositeUseCase[P, R]): List[UseCase[P, R]] = t.allUseCases
  }
}
class CompositeUseCase[P, R](val allUseCases: List[UseCase[P, R]], val data: EngineComponentData) extends UseCase[P, R] {
  val allScenarios: List[Scenario[P, R]] = allUseCases.flatMap(_.allScenarios)
  def or(useCase1: UseCase1[P, R]) = new CompositeUseCase[P, R](allUseCases :+ useCase1, EngineComponentData(DefinedInSourceCodeAt.definedInSourceCodeAt(), None))
  override def copyWithOnly(list: List[Scenario[P, R]]): UseCase[P, R] = new CompositeUseCase(allUseCases.map(_.copyWithOnly(list)),data)

}

