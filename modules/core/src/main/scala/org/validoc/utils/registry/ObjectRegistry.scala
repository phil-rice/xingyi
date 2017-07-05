package org.validoc.utils.registry

import java.util.concurrent.atomic.AtomicReference
import java.util.function.UnaryOperator

import scala.collection.concurrent.TrieMap
import scala.reflect.ClassTag


object ObjectRegistry {
  def register[T: ClassTag](value: ObjectRegistry[T]): Int = {
    ref.updateAndGet(new UnaryOperator[List[Any]] {
      override def apply(t: List[Any]): List[Any] = value :: t
    }).size
  }

  def all[T: ClassTag] = ref.get

  private def ref[T: ClassTag] = map.getOrElseUpdate(implicitly[ClassTag[T]].runtimeClass, new AtomicReference(List()))

  private val map = TrieMap[Class[_], AtomicReference[List[Any]]]()
}

abstract class ObjectRegistry[T: ClassTag] {
  self: T =>
  val registryId = ObjectRegistry.register[T](this)
}
