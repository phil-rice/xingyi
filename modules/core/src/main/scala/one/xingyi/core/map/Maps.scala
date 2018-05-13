package one.xingyi.core.map

object Maps {

  def addTo[K, V](map: Map[K, Seq[V]], k: K, v: V) = (map + (k -> map.get(k).fold(Seq(v))(seq => seq ++ Seq(v))))
  def mergeAll[K, V](maps: Seq[Map[K, Seq[V]]]): Map[K, Seq[V]] = {
    val allKeys = maps.flatMap(_.keySet)
    allKeys.map(k => (k, maps.flatMap(m => m.getOrElse(k, Nil)))).toMap[K, Seq[V]]
  }
}
