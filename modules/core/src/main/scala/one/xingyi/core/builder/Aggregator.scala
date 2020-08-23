/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.builder

import java.util.concurrent.atomic.AtomicReference

import one.xingyi.core.optics.Lens

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
object HasId {
  implicit def fromLens[T, Id](implicit lens: IdLens[T, Id]): HasId[T, Id] = { t => lens.get(t) }
}
trait CopyWithNewId[T, ID] extends ((ID, T) => T)
object CopyWithNewId {
  implicit def fromLens[T, Id](implicit lens: IdLens[T, Id]): CopyWithNewId[T, Id] = { (id, t) => lens.set(t, id) }
}
case class IdLens[T, ID](get:T => ID, set: (T, ID) => T) extends Lens[T, ID]

class RememberingAggregator2[T, ID](implicit hasId: HasId[T, ID]) extends Aggregator[T] {
  private val list = new AtomicReference[List[T]](List())
  def items = list.get.reverse
  override def apply(comp: T): Unit = list.updateAndGet(list => comp :: list.filterNot(c => hasId(c) == hasId(comp)))
  def clear() = list.updateAndGet(_ => List())
}
