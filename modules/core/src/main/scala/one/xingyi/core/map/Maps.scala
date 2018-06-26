package one.xingyi.core.map

object Maps {

  def addTo[K, V](map: Map[K, Seq[V]], k: K, v: V) = (map + (k -> map.get(k).fold(Seq(v))(seq => seq ++ Seq(v))))
  def mergeAll[K, V](maps: Seq[Map[K, Seq[V]]]): Map[K, Seq[V]] = {
    val allKeys = maps.flatMap(_.keySet)
    allKeys.map(k => (k, maps.flatMap(m => m.getOrElse(k, Nil)))).toMap[K, Seq[V]]
  }

  implicit class MapOfListsPimper[K, V](map: Map[K, List[V]]) {
    def addToList(kv: (K, V)): Map[K, List[V]] = kv match {case (k, v) => map.get(k).fold(map + (k -> List[V](v)))(list => map + (k -> (list :+ v)))}
    def items(id: K): List[V] = map.getOrElse(id, Nil)
  }
  implicit class ToMapOps[V](list: List[V]) {
    def toMapFrom[K](fn: V => K) = list.foldLeft(Map[K, V]())((acc, v) => acc + (fn(v) -> v))
  }

}
