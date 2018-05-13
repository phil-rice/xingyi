package one.xingyi.core.local

import one.xingyi.core.language.Language._

import scala.collection.concurrent.TrieMap
import scala.concurrent.Future
import scala.language.higherKinds
import scala.reflect.ClassTag

trait LocalOps[M[_]] {
  def get[V: ClassTag](): Option[V]
  def put[V: ClassTag](v: V)
  def clear[V: ClassTag]()
}

object LocalOps {
  implicit val localOpsForScalaFuture: LocalOps[Future] = LocalOpsForScalaFuture.localOpsForScalaFuture
}

trait Holder[H[_]] {
  def makeHolder[V: ClassTag]: H[V]
  def getValueOutOfHolder[V](holder: H[V]): Option[V]
  def putValueInHolder[V](v: Option[V])(holder: H[V]): Unit
}

class SimpleLocalOps[M[_], H[_]](holder: Holder[H]) extends LocalOps[M] {
  import holder._
  val map = new TrieMap[Class[_], H[_]]
  def key[V: ClassTag]: Class[_] = implicitly[ClassTag[V]].runtimeClass
  def getHolder[V: ClassTag]: H[V] = map.getOrElseUpdate(key[V], holder.makeHolder[V]).asInstanceOf[H[V]]

  override def get[V: ClassTag](): Option[V] = getHolder[V] |> getValueOutOfHolder
  override def put[V: ClassTag](v: V): Unit = getHolder[V] |> putValueInHolder(Some(v))
  override def clear[V: ClassTag](): Unit = getHolder[V] |> putValueInHolder(None)
}

trait LocalOpsPimper[M[_]] {
  protected def getFromLocalStore[V: ClassTag]()(implicit localOps: LocalOps[M]): Option[V] = localOps.get[V]
  protected def putInlocalStore[V: ClassTag](v: V)(implicit localOps: LocalOps[M]): Unit = localOps.put[V](v)
  protected def clearlocalStore[V: ClassTag]()(implicit localOps: LocalOps[M]): Unit = localOps.clear[V]
}

