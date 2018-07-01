/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.map
import one.xingyi.core.builder.HasId

object Maps {

  def addTo[K, V](map: Map[K, Seq[V]], k: K, v: V) = (map + (k -> map.get(k).fold(Seq(v))(seq => seq ++ Seq(v))))
  def mergeAll[K, V](maps: Seq[Map[K, Seq[V]]]): Map[K, Seq[V]] = {
    val allKeys = maps.flatMap(_.keySet)
    allKeys.map(k => (k, maps.flatMap(m => m.getOrElse(k, Nil)))).toMap[K, Seq[V]]
  }

  implicit class MapOps[K, V](map: Map[K, V]) {
    def optAdd(tuples: (K, Option[V])*) = tuples.foldLeft(map)((acc, tuple) => tuple._2.fold(acc)(v => acc + (tuple._1 -> v)))
    def add(v: V)(implicit hasId: HasId[V, K]) = map + (hasId(v) -> v)
  }
  implicit class MapOfListsPimper[K, V](map: Map[K, List[V]]) {
    def addToList(kv: (K, V)): Map[K, List[V]] = kv match {case (k, v) => map.get(k).fold(map + (k -> List[V](v)))(list => map + (k -> (list :+ v)))}
    def items(id: K): List[V] = map.getOrElse(id, Nil)
  }
  implicit class ToMapOps[V](list: List[V]) {
    def toMapFrom[K](fn: V => K) = list.foldLeft(Map[K, V]())((acc, v) => acc + (fn(v) -> v))
  }

}
