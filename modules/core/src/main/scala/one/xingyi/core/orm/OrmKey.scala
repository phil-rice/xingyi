package one.xingyi.core.orm

import java.io.{ByteArrayOutputStream, OutputStream}

import one.xingyi.core.aggregate.HasChildrenForHolder
import one.xingyi.core.logging.LoggingAdapter
import one.xingyi.core.orm.SchemaMapKey._
import one.xingyi.core.strings.Strings

import scala.annotation.implicitNotFound
import scala.collection.immutable.List
import scala.language.higherKinds

sealed trait ChildArity
case object NoChildren extends ChildArity
case object OneChild extends ChildArity
case object ManyChildren extends ChildArity

sealed abstract class ChildrenInSchema[Schema[_]](val arity: ChildArity) {def children: List[Schema[_]]}
case class Zero[Schema[_]]() extends ChildrenInSchema[Schema](NoChildren) {def children: List[Schema[_]] = Nil}
case class AlwaysOne[Schema[_]](children: List[Schema[_]]) extends ChildrenInSchema[Schema](OneChild)
case class ZeroOrMore[Schema[_]](children: List[Schema[_]]) extends ChildrenInSchema[Schema](ManyChildren)

trait Placeholder

/** THere is a schema of type Schema. Schema is not the data, is the structure of the data
 *
 * The type parameter is the type of the data in the field that the schema is pointing to.
 * This is used for things like 'getting from database' and 'putting in json'.
 *
 * Because schemas hold other schemas, and I am not sure how to handle the schema object itself from a [T] perspective
 * there is a fake T called Placeholder. */
trait SchemaMapKey[Schema[_]] {
  def childKey[T](t: Schema[T]): String // The main object might not have a key, but the children will
  def children[T](t: Schema[T]): ChildrenInSchema[Schema]
  def descendants[T](t: Schema[T]): List[Schema[_]] = {
    val c: List[Schema[_]] = children(t).children
    c ::: c.flatMap(descendants(_))
  }
}

object SchemaMapKey {
  implicit class SchemaMapKeyOps[S[_], T](s: S[T])(implicit k: SchemaMapKey[S]) {
    def key: String = k.childKey(s)
    def children: ChildrenInSchema[S] = k.children(s)
    def descendants: List[S[_]] = k.descendants(s)
  }
}

trait FieldFilter[F[_]] {
  def apply[T](f: F[T]): Boolean
  def filtered(it: Iterable[F[_]]): List[F[_]] = it.filter(apply(_)).toList
}
trait IsLinkFieldFilter[F[_]] extends FieldFilter[F]


trait IsObjectFieldFilter[F[_]] extends FieldFilter[F]
object IsObjectFieldFilter {
  implicit def isObject[F[_]](implicit hasChildren: HasChildrenForHolder[F]): IsObjectFieldFilter[F] = new IsObjectFieldFilter[F] {
    override def apply[T](f: F[T]): Boolean = hasChildren(f).nonEmpty
  }
}

trait TableNameForManySchema[Schema[_]] {
  def apply[T](s: Schema[T]): Option[TableName]
}

object TableNameForManySchema {
  def apply[S[_]](keysToTableNames: Map[String, TableName])(implicit schemaMapKey: SchemaMapKey[S]): TableNameForManySchema[S] = new TableNameForManySchema[S] {
    override def apply[T](s: S[T]): Option[TableName] = keysToTableNames.get(s.key)
  }
}

trait IsSimpleFieldFilter[F[_]] extends FieldFilter[F]
object IsSimpleFieldFilter {
  implicit def isSimple[F[_]](implicit isLink: IsLinkFieldFilter[F], isObject: IsObjectFieldFilter[F]): IsSimpleFieldFilter[F] = new IsSimpleFieldFilter[F] {
    override def apply[T](f: F[T]): Boolean = !(isLink(f) || isObject(f))
  }
}
