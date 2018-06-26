package one.xingyi.core.orm

import one.xingyi.core.map.Maps._
trait OrmMaker[T] extends (Map[OrmEntity, List[List[AnyRef]]] => List[T])

object OrmMaker {
  implicit class ListofVOps[K, V](list: List[V]) {
    def mapPf[T](pf: PartialFunction[V, T]) = list.map {pf orElse { case x => throw new RuntimeException(s"Unexpected issue. Cannot match $x") }}
  }
  def toMap[X](list: List[List[AnyRef]])(fn: List[AnyRef] => X): Map[Any, List[X]] =
    list.foldLeft[Map[Any, List[X]]](Map()) { case (acc, key :: _ :: values) => acc addToList key -> fn(values); case x => throw new RuntimeException(s"Unexpected issue in fold. Cannot match $x") }
  def str(n: Int)(implicit list: List[AnyRef]): String = list(n).toString

}

trait OrmEntity {
  def tableName: String
  def alias: String
  def primaryKeyField: FieldType[Int]
  def dataFields: List[FieldType[_]]
  def children: List[ChildEntity]
  def fieldsForCreate: List[FieldType[_]]
}

trait ChildEntity extends OrmEntity

case class MainEntity(tableName: String, alias: String, primaryKeyField: FieldType[Int], dataFields: List[FieldType[_]], children: List[ChildEntity]) extends OrmEntity {
  val fieldsForCreate = primaryKeyField :: dataFields
}
case class OneToManyEntity(tableName: String, alias: String, primaryKeyField: FieldType[Int], parentId: FieldType[Int], dataFields: List[FieldType[_]], children: List[ChildEntity]) extends ChildEntity {
  val fieldsForCreate = primaryKeyField :: parentId :: dataFields
}
