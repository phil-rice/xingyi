package org.validoc.utils.aggregate

import scala.collection.concurrent.TrieMap
import scala.reflect.ClassTag
import scala.language.higherKinds
trait LocalOps {
  def get[V: ClassTag](): Option[V]
  def put[V: ClassTag](v: V)
  def clear[V: ClassTag]()
}

trait Holder[H[_]] {
  def makeHolder[V: ClassTag]: H[V]
  def getValueOutOfHolder[V](holder: H[V]): Option[V]
  def putValueInHolder[V](v: Option[V])(holder: H[V]): Unit

}

class SimpleLocalOps[H[_]](holder: Holder[H]) extends LocalOps with AnyPimpers {

  import holder._

  val map = new TrieMap[Class[_], H[_]]
  def key[V: ClassTag]: Class[_] = implicitly[ClassTag[V]].runtimeClass
  def getHolder[V: ClassTag]: H[V] = map.getOrElseUpdate(key[V], holder.makeHolder[V]).asInstanceOf[H[V]]

  override def get[V: ClassTag](): Option[V] = getHolder[V] |> getValueOutOfHolder
  override def put[V: ClassTag](v: V): Unit = getHolder[V] |> putValueInHolder(Some(v))
  override def clear[V: ClassTag](): Unit = getHolder[V] |> putValueInHolder(None)
}

trait LocalOpsPimper {

  def getFromLocalStore[V: ClassTag]()(implicit localOps: LocalOps): Option[V] = localOps.get[V]
  def putInlocalStore[V: ClassTag](v: V)(implicit localOps: LocalOps): Unit = localOps.put[V](v)
  def clearlocalStore[V: ClassTag]()(implicit localOps: LocalOps): Unit = localOps.clear[V]

  def getOrCreateLocalStore[V: ClassTag](default: => V)(implicit localOps: LocalOps): V = localOps.get[V].getOrElse {
    val v = default
    localOps.put(v)
    v
  }
  def modifyLocalStore[V: ClassTag](fn: V => V)(implicit localOps: LocalOps): Unit = getFromLocalStore[V]().foreach(v => putInlocalStore(fn(v)))
  def useLocalStore[V: ClassTag](fn: V => Unit)(implicit localOps: LocalOps): Unit = getFromLocalStore[V].foreach(fn)
}

object LocalOpsPimper extends LocalOpsPimper