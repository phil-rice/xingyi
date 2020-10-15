package one.xingyi.core.orm

import one.xingyi.core.aggregate.HasChildrenForHolder
import one.xingyi.core.orm.SchemaMapKey._

import scala.collection.immutable.List
import scala.language.higherKinds


/** THere is a schema of type Schema. Schema is not the data, is the structure of the data
 *
 * The type parameter is the type of the data in the field that the schema is pointing to.
 * This is used for things like 'getting from database' and 'putting in json'.
 *
 * Because schemas hold other schemas, and I am not sure how to handle the schema object itself from a [T] perspective
 * there is a fake T called Placeholder. */
trait SchemaMapKey[Schema[_]] {
  def childKey[T](t: Schema[T]): String // The main object might not have a key, but the children will
  def children[T](t: Schema[T]): List[Schema[_]]
  def descendants[T](t: Schema[T]): List[Schema[_]] = {
    children(t) ::: children(t).flatMap(descendants(_))
  }
}

object SchemaMapKey {
  implicit class SchemaMapKeyOps[S[_], T](s: S[T])(implicit k: SchemaMapKey[S]) {
    def key: String = k.childKey(s)
    def children: List[S[_]] = k.children(s)
    def descendants: List[S[_]] = k.descendants(s)
  }
}

trait ToTableAndFieldTypes[Context, Schema[_]] {def apply[T](s: Schema[T]): List[TableAndFieldTypes[Context, T]]}

trait Placeholder

trait FieldFilter[F[_]] {
  def apply[T](f: F[T]): Boolean
  def filtered(it: Iterable[F[_]]): List[F[_]] = it.filter(apply(_)).toList
}

trait IsLinkFieldFilter[F[_]] extends FieldFilter[F]
trait ArrayTableName[F[_]] {
  def apply(f: F[_]): Option[TableName]
}
case class ArrayTableNameFromMap[S[_] : SchemaMapKey](map: Map[String, TableName]) extends ArrayTableName[S] {
  override def apply(f: S[_]): Option[TableName] = map.get(f.key)
}

trait IsObjectFieldFilter[F[_]] extends FieldFilter[F]
object IsObjectFieldFilter {
  implicit def isObject[F[_]](implicit hasChildren: HasChildrenForHolder[F], arrayFieldFilter: ArrayTableName[F]): IsObjectFieldFilter[F] =
    new IsObjectFieldFilter[F] {override def apply[T](f: F[T]): Boolean = hasChildren(f).nonEmpty && arrayFieldFilter(f).isEmpty}
}

trait IsSimpleFieldFilter[F[_]] extends FieldFilter[F]
object IsSimpleFieldFilter {
  implicit def isSimple[F[_] : SchemaMapKey](implicit isLink: IsLinkFieldFilter[F]): IsSimpleFieldFilter[F] =
    new IsSimpleFieldFilter[F] {override def apply[T](f: F[T]): Boolean = f.children.isEmpty && !isLink(f)}
}


