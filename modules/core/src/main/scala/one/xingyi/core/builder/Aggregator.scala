package one.xingyi.core.builder

import java.util.concurrent.atomic.AtomicReference

import scala.language.higherKinds
sealed trait YesNo
trait Yes extends YesNo
trait No extends YesNo

trait HasAggregator[T] {
  def aggregator: Aggregator[T]
}
trait Aggregator[T] extends (T => Unit)
object Aggregator {
  def nullAggregator[T] = new Aggregator[T] {
    override def apply(v1: T): Unit = {}
  }
}
trait HasId[T, ID] extends (T => ID)

class RememberingAggregator2[T, ID](implicit hasId: HasId[T, ID]) extends Aggregator[T] {
  private val list = new AtomicReference[List[T]](List())
  def items = list.get.reverse
  override def apply(comp: T): Unit = list.updateAndGet(list => comp :: list.filterNot(c => hasId(c) == hasId(comp)))
  def clear() = list.updateAndGet(_ => List())
}
