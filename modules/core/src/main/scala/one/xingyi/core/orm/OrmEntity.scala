/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.orm

import one.xingyi.core.map.Maps._

trait OrmMaker[T] extends (Map[OrmEntity, List[List[AnyRef]]] => Stream[T])

object OrmMaker {
  implicit class ListofVOps[K, V](list: List[V]) {
    def mapPf[T](pf: PartialFunction[V, T]): Stream[T] = list.toStream.map { pf orElse { case x => throw new RuntimeException(s"Unexpected issue. Cannot match $x") } }
  }
  def toMapForOneToMany[X](list: List[List[AnyRef]])(fn: List[AnyRef] => X): Map[Any, List[X]] =
    list.foldLeft[Map[Any, List[X]]](Map()) { case (acc, key :: _ :: values) => acc addToList key -> fn(values); case x => throw new RuntimeException(s"Unexpected issue in fold. Cannot match $x") }

  def toMapForManyToOne[X](list: List[List[AnyRef]])(fn: List[AnyRef] => X): Map[Any, X] = {
    list.foldLeft[Map[Any, X]](Map()) { case (acc, key :: values) => acc + (key -> fn(values)); case x => throw new RuntimeException(s"Unexpected issue in fold. Cannot match $x") }
  }
  def str(n: Int)(implicit list: List[AnyRef]): String = list(n).toString

}

trait OrmEntity {
  def tableName: String
  def alias: String
  def primaryKeyField: FieldType
  def dataFields: List[FieldType]
  def children: List[ChildEntity]
  def fieldsForCreate: List[FieldType]
  def fieldsAddedByChildren: List[FieldType] = children.flatMap(_.parentFields)
}

sealed trait ChildEntity extends OrmEntity {
  def parentFields: List[FieldType]
}

case class MainEntity(tableName: String, alias: String, primaryKeyField: FieldType, dataFields: List[FieldType], children: List[ChildEntity]) extends OrmEntity {
  val fieldsForCreate = primaryKeyField :: fieldsAddedByChildren ::: dataFields
}
case class OneToManyEntity(tableName: String, alias: String, primaryKeyField: FieldType, parentId: FieldType, dataFields: List[FieldType], children: List[ChildEntity]) extends ChildEntity {
  val fieldsForCreate = primaryKeyField :: parentId :: dataFields
  override def parentFields: List[FieldType] = List()
}
case class ManyToOneEntity(tableName: String, alias: String, primaryKeyField: FieldType, idInParent: FieldType, dataFields: List[FieldType], children: List[ChildEntity]) extends ChildEntity {
  val fieldsForCreate = primaryKeyField :: dataFields
  override def parentFields: List[FieldType] = List(idInParent)
}