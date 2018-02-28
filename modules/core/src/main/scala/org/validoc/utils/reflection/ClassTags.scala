package org.validoc.utils.reflection

import scala.collection.IterableLike
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.List
import scala.reflect.ClassTag

trait ClassTags {
  def clazz[T](implicit classTag: ClassTag[T]) = classTag.runtimeClass
  def nameOf[T](implicit classTag: ClassTag[T]) = classTag.runtimeClass.getSimpleName

  def isA[T](t: Any)(implicit classTag: ClassTag[T]) = classTag.runtimeClass.isAssignableFrom(t.getClass)

  def collectAll[T: ClassTag](i: Seq[Any]): Seq[T] = i.collect { case r if isA[T](r) => r.asInstanceOf[T] }

  implicit class AnyPimperForClassTags(a: Any) {
    def is[T: ClassTag]: Boolean = isA[T](a)
  }

  implicit class IterablePimperForClassTags(s: Seq[Any]) {
    def collectAs[T: ClassTag]: Seq[T] = collectAll[T](s)
  }

}

object ClassTags extends ClassTags
