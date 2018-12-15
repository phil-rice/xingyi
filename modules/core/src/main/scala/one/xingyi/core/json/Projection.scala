package one.xingyi.core.json
import one.xingyi.core.strings.Strings

import scala.reflect.ClassTag

sealed trait Projection[T] {
  def classTag: ClassTag[T]
  def toJson(t: T): JsonValue
}

case class ObjectProjection[T](children: (String, FieldProjection[T, _])*)(implicit val classTag: ClassTag[T]) extends Projection[T] {
  override def toJson(t: T): JsonValue = JsonObject(children.map { nameAndToChild => nameAndToChild._1 -> nameAndToChild._2.childJson(t) }: _*)
}

sealed trait FieldProjection[T, Child] {
  def childJson(t: T): JsonValue
}

case class ObjectFieldProjection[T, Child](fn: T => Child)(implicit val projection: ObjectProjection[Child], val classTag: ClassTag[Child]) extends FieldProjection[T, Child] {
  override def childJson(t: T): JsonValue = JsonObject(projection.children.map { nameAndToChild => nameAndToChild._1 -> nameAndToChild._2.childJson(fn(t)) }: _*)
}

case class ListFieldProjection[T, Child](fn: T => List[Child])(implicit val projection: Projection[Child], val classTag: ClassTag[Child]) extends FieldProjection[T, List[Child]] {
  override def childJson(t: T): JsonValue = JsonList(fn(t).map(projection.toJson))
}
case class StringFieldProjection[T](fn: T => String) extends FieldProjection[T, String] {
  override def childJson(t: T): JsonValue = JsonString(fn(t))
}