package one.xingyi.core.map

import one.xingyi.core.concurrency.DoubleCheckLock

/** reasonably thread safe singleton map. 'reasonably' because if the makeT method has locks we might end up in a deadlock.
 * If the makeT doesn't lock then it's safe, and guarantees that only one singleton for each key exists */
object SingletonMap {
  def apply[K, T](makeT: K => T): SingletonMap[K, T] = new SingletonMap[K, T](makeT)
}
class SingletonMap[K, T](makeT: K => T) extends (K => T) {
  val lock = new DoubleCheckLock()
  var map: Map[K, T] = Map()
  override def apply(key: K): T = {
    lock(!map.contains(key)){map = map + (key -> makeT(key))}
    map(key)
  }
}
