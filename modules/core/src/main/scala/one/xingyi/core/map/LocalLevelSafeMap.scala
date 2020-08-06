/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.map

import java.util.concurrent.atomic.AtomicReference
import java.util.function.UnaryOperator

import one.xingyi.core.concurrency.DoubleCheckLock

import scala.language.higherKinds

trait ThreadSafeDataManips[M[_], K, V] {
  def size(_map: Map[K, M[V]]): Int

  def foldLeft[Acc](map: Map[K, M[V]], initial: Acc, fn: (Acc, (K, V)) => Acc): Acc

  def empty(): Map[K, M[V]]

  def contains(map: Map[K, M[V]], k: K): Boolean

  def put(map: Map[K, M[V]], k: K, v: => V): Map[K, M[V]]

  def get(map: Map[K, M[V]], k: K): V

  def update(map: Map[K, M[V]], k: K, fn: V => V): Map[K, M[V]]
}

class AtomicRefDataManips[K, V] extends ThreadSafeDataManips[AtomicReference, K, V] {
  override def empty(): Map[K, AtomicReference[V]] = Map[K, AtomicReference[V]]()

  override def put(map: Map[K, AtomicReference[V]], k: K, v: => V): Map[K, AtomicReference[V]] = {
    map + (k -> new AtomicReference[V](v))
  }

  override def update(map: Map[K, AtomicReference[V]], k: K, fn: (V) => V): Map[K, AtomicReference[V]] = {
    map(k).updateAndGet(new UnaryOperator[V] {
      override def apply(t: V) = fn(t)
    })
    map
  }

  override def contains(map: Map[K, AtomicReference[V]], k: K): Boolean = map.contains(k)

  override def get(map: Map[K, AtomicReference[V]], k: K): V = map(k).get

  //  override def foldLeft[Acc](map: Map[K, AtomicReference[V]], initial: Acc, fn: (Acc, (K, V)) => Acc): Unit = map.foldLeft(initial) { case (acc, (k, mv)) => fn(acc, (k, mv.get)) }
  override def size(_map: Map[K, AtomicReference[V]]): Int = _map.size

  override def foldLeft[Acc](map: Map[K, AtomicReference[V]], initial: Acc, fn: (Acc, (K, V)) => Acc): Acc = map.foldLeft(initial) { case (acc, (k, mv)) => fn(acc, (k, mv.get)) }
}

/** This is used for things like thread safe caches. This is thread safe. You can get a value or change a value.
  *
  * The initial value if the item isn't in, is not specified here: it's specified in the implementing class
  */
trait SafeMap[K, V] {
  def apply(k: K)(fn: V => V): V

  def get(k: K): V

  def foldLeft[Acc](initial: Acc)(fn: (Acc, (K, V)) => Acc): Acc

  def size: Int
}

object SafeMap {
  def apply[K, V](default: => V, mapSizeStrategy: MapSizeStrategy = NoMapSizeStrategy, reportMapSizeReduction: ReportMapSizeReduction = NoReportMapSizeReduction) =
    new LowLevelSafeMap[AtomicReference, K, V](new AtomicRefDataManips, default, mapSizeStrategy,reportMapSizeReduction)
}

class LowLevelSafeMap[M[_], K, V](threadSafeDataManips: ThreadSafeDataManips[M, K, V], default: => V, mapSizeStrategy: MapSizeStrategy, reportMapSizeReduction: ReportMapSizeReduction) extends SafeMap[K, V] {

  import threadSafeDataManips._

  private val initialValueLock = new DoubleCheckLock
   var _map = empty()

  def clear = initialValueLock(true)(_map = empty())

//  def copyOfMap: Map[K, M[V]] = _map


  private def makeSureKExists(k: K) = {
    initialValueLock(!contains(_map, k)) {
      _map = put(mapSizeStrategy.modifyCache(_map, reportMapSizeReduction), k, default)
    }
  }

  def get(k: K): V = {
    makeSureKExists(k)
    threadSafeDataManips.get(_map, k)
  }

  def apply(k: K)(fn: V => V): V = {
    makeSureKExists(k)
    update(_map, k, fn)
    threadSafeDataManips.get(_map, k)
  }

  override def foldLeft[Acc](initial: Acc)(fn: (Acc, (K, V)) => Acc): Acc = threadSafeDataManips.foldLeft(_map, initial, fn)

  override def size: Int = threadSafeDataManips.size(_map)
}
