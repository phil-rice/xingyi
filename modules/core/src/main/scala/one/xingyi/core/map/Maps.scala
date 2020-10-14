/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.map

import one.xingyi.core.builder.HasId

import scala.collection.mutable

object Maps {

  def addTo[K, V](map: Map[K, Seq[V]], k: K, v: V) = (map + (k -> map.get(k).fold(Seq(v))(seq => seq ++ Seq(v))))
  def mergeAll[K, V](maps: Seq[Map[K, Seq[V]]]): Map[K, Seq[V]] = {
    val allKeys = maps.flatMap(_.keySet)
    allKeys.map(k => (k, maps.flatMap(m => m.getOrElse(k, Nil)))).toMap[K, Seq[V]]
  }


  implicit class MapOps[K, V](map: Map[K, V]) {
    def addIf(b: Boolean, tuple: (K, V)) = if (b) map + tuple else map
    def optAdd(tuples: (K, Option[V])*) = tuples.foldLeft(map)((acc, tuple) => tuple._2.fold(acc)(v => acc + (tuple._1 -> v)))
    def add(v: V)(implicit hasId: HasId[V, K]) = map + (hasId(v) -> v)
  }
  implicit class MapOfListsOps[K, V](map: Map[K, List[V]]) {
    def addToList(kv: (K, V)): Map[K, List[V]] = kv match {case (k, v) => map.get(k).fold(map + (k -> List[V](v)))(list => map + (k -> (list :+ v)))}
    def items(id: K): List[V] = map.getOrElse(id, Nil)
    //    def toJavaMapOfLists = map.foldLeft(new util.HashMap[K, java.util.ArrayList[V]]) { case (acc, (k, l)) => acc.put(k, l.foldLeft(new util.ArrayList[V]()) { case (acc2, v) => acc2.add(v); acc2 }); acc }
  }

  implicit class ToMapOps[V](list: List[V]) {
    def toMapFrom[K](fn: V => K) = list.foldLeft(Map[K, V]())((acc, v) => acc + (fn(v) -> v))
  }
  implicit class IteratorOfKVOps[K, V](list: Iterator[(K, V)]) {
    def toMapOfLists: Map[K, List[V]] = list.foldLeft[Map[K, List[V]]](Map()) { case (acc, kv) => acc.addToList(kv) }
  }
  implicit class IterableOfKVOps[K, V](list: Iterable[(K, V)]) {
    def toMapOfLists: Map[K, List[V]] = list.foldLeft[Map[K, List[V]]](Map()) { case (acc, kv) => acc.addToList(kv) }
  }

  def toJavaRecursively(a: Any): AnyRef = a match {
    case v: Map[_, _] => toJavaMapRecursively(v)
    case v: List[_] => toJavaListRecursively(v)
    case _ => a.asInstanceOf[AnyRef]
  }

  def toJavaListRecursively(list: List[_]): java.util.List[AnyRef] = {
    val result = new java.util.ArrayList[Object](list.size)
    list.foreach(a => result.add(toJavaRecursively(a)))
    result
  }

  def toJavaMapRecursively(map: Map[_, _]): java.util.Map[String, Object] = {
    val result = new java.util.HashMap[String, Object](map.size)
    map.foreach(kv => result.put(kv._1.toString, toJavaRecursively(kv._2)))
    result
  }
}
