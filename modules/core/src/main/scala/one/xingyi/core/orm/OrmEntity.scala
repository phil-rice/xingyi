/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.orm

import one.xingyi.core.map.Maps._

trait OrmMaker[T] extends (Map[OrmEntity, List[List[AnyRef]]] => Stream[T])

object OrmMaker {
  implicit class ListofVOps[K, V](list: List[V]) {
    def mapPf[T](pf: PartialFunction[V, T]) = list.toStream.map {pf orElse { case x => throw new RuntimeException(s"Unexpected issue. Cannot match $x") }}
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
