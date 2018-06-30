package one.xingyi.core.builder

import scala.language.higherKinds
sealed trait YesNo
trait Yes extends YesNo
trait No extends YesNo

trait HasAggregator[T] {
  def aggregator: Aggregator[T]
}
trait Aggregator[T] extends (T => Unit)
trait HasId[T, ID] extends (T => ID)

class RememberingAggregator2[T](implicit hasId: HasId[T, Int]) extends Aggregator[T] {
  private var list = List[T]()
  private val lock = new Object()
  def items = list
  override def apply(comp: T): Unit = list = list.filterNot(c => hasId(c) == hasId(comp)) :+ comp
  def clear = list = List()
}
