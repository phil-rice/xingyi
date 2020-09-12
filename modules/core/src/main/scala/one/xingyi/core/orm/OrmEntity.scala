/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.orm

import one.xingyi.core.json.JsonObject
import one.xingyi.core.map.Maps._


trait OrmMaker[T] extends (MainEntity => Map[OrmEntity, List[List[AnyRef]]] => Stream[T])

object OrmMaker {
  type RawOrmMap = Map[OrmEntity, List[List[AnyRef]]]
  type MakeTFn[T] = OrmEntity => List[Any] => T

  def toMapOfIdToList(map: RawOrmMap): Map[OrmEntity, Map[Any, List[AnyRef]]] =
    map.mapValues(_.map { case head :: rest => head -> rest; case _ => throw new RuntimeException() }.toMap)

  def toListofIdAnd[T](map: RawOrmMap, entity: OrmEntity)(fn: MakeTFn[T]): List[(Any, T)] =
    map(entity).map { case id :: rest => id -> fn(entity)(rest); case _ => throw new RuntimeException() }

  def toMapofIdTo[T](map: RawOrmMap, entity: OrmEntity)(fn: MakeTFn[T]): Map[Any, T] =
    toListofIdAnd(map, entity)(fn).toMap


  def toJson(entity: OrmEntity)(list: List[Any]): JsonObject = {
    println(s"OrmMaker.toJson $list  rows to drop ${entity.colsToDropForValues}")
    JsonObject(list.drop(entity.colsToDropForValues).zip(entity.dataFields).map {
      case (value, df) => df.name -> df.toJson(value)
    }: _*)
  }

  def toIdAndJson(entity: OrmEntity)(list: List[AnyRef]): (String, JsonObject) = list match {
    case primaryKey :: (rest: List[AnyRef]) => (primaryKey.toString, toJson(entity)(list))
    case _ => throw new RuntimeException("Cannot convert toIdAndJson")
  }

  def toJson(entity: OrmEntity, map: Map[OrmEntity, List[List[AnyRef]]]): List[(String, JsonObject)] = {
    val dataJsonObject: List[(String, JsonObject)] = map(entity).map(toIdAndJson(entity))
    val childJsonObjects: List[(String, JsonObject)] = entity.children.map(child => child.tableName -> JsonObject(toJson(child, map): _*))
    dataJsonObject ::: childJsonObjects

  }


  implicit object OrmMakerForJson extends OrmMaker[JsonObject] {
    override def apply(main: MainEntity): Map[OrmEntity, List[List[AnyRef]]] => Stream[JsonObject] = {
      map =>
        val list = toJson(main, map).map(_._2)
        println("results")
        list.foreach(println)
        println("end")
        list.toStream
    }
  }

  implicit class ListofVOps[K, V](list: List[V]) {
    def mapPf[T](pf: PartialFunction[V, T]): Stream[T] = list.toStream.map { pf orElse { case x => throw new RuntimeException(s"Unexpected issue. Cannot match $x") } }
  }
  def toMapForOneToMany[X](list: List[List[AnyRef]])(fn: List[AnyRef] => X): Map[Any, List[X]] =
    list.foldLeft[Map[Any, List[X]]](Map()) {
      case (acc, key :: _ :: values) => acc addToList key -> fn(values);
      case x => throw new RuntimeException(s"Unexpected issue in fold. Cannot match $x")
    }

  def toMapForManyToOneAndSameId[X](list: List[List[AnyRef]])(fn: List[AnyRef] => X): Map[Any, X] = {
    list.foldLeft[Map[Any, X]](Map()) {
      case (acc, key :: values) => acc + (key -> fn(values));
      case x => throw new RuntimeException(s"Unexpected issue in fold. Cannot match $x")
    }
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
  def colsToDropForValues = fieldsForCreate.size - dataFields.size
  protected def childrenPrettyString(indent: String) = if (children.isEmpty) "" else children.map(_.prettyPrint(indent + "  ")).mkString("{\n", "\n", s"$indent\n}")
  protected def fieldsPrettyString = {
    val data = s"data=${dataFields.map(_.name).mkString(",")}"
    val addedByChildren = if (fieldsAddedByChildren.isEmpty) "" else s"childrenAdded=${fieldsAddedByChildren.map(_.name).mkString(",")}, "
    addedByChildren + data
  }
  def prettyPrint(indent: String): String
}

sealed trait ChildEntity extends OrmEntity {
  def parentFields: List[FieldType]
}


case class MainEntity(tableName: String, alias: String, primaryKeyField: FieldType, dataFields: List[FieldType], children: List[ChildEntity]) extends OrmEntity {
  val fieldsForCreate = primaryKeyField :: fieldsAddedByChildren ::: dataFields

  def stream[T](batchConfig: OrmBatchConfig)(implicit ormMaker: OrmMaker[T], fastReaderOps: FastReaderDal, sqlOps: FastOrmSql): Stream[T] = stream(FastReader(batchConfig), 0)
  private def stream[T](fastReader: FastReader[T], n: Int): Stream[T] = {
    val subStream: Stream[T] = fastReader(this)(n)
    if (subStream.isEmpty) subStream else subStream #::: stream(fastReader, n + 1)
  }
  override def prettyPrint(i: String) = s"${i}MainEntity($tableName, id=${primaryKeyField.name}, $fieldsPrettyString)${childrenPrettyString(i)}"
}
case class OneToManyEntity(tableName: String, alias: String, primaryKeyField: FieldType, parentId: FieldType, dataFields: List[FieldType], children: List[ChildEntity]) extends ChildEntity {
  val fieldsForCreate = primaryKeyField :: parentId :: dataFields
  override def parentFields: List[FieldType] = List()
  override def prettyPrint(i: String) = s"${i}OneToMany($tableName, id=${primaryKeyField.name}, parent=${parentId.name} $fieldsPrettyString)${childrenPrettyString(i)}"
}
case class ManyToOneEntity(tableName: String, alias: String, primaryKeyField: FieldType, idInParent: FieldType, dataFields: List[FieldType], children: List[ChildEntity]) extends ChildEntity {
  val fieldsForCreate = primaryKeyField :: dataFields
  override def parentFields: List[FieldType] = List(idInParent)
  override def prettyPrint(i: String) = s"${i}ManyToOne($tableName, id=${primaryKeyField.name}, idInParent=${idInParent.name} $fieldsPrettyString)${childrenPrettyString(i)}"
}
case class SameIdEntity(tableName: String, alias: String, primaryKeyField: FieldType, dataFields: List[FieldType], children: List[ChildEntity]) extends ChildEntity {
  override val fieldsForCreate = primaryKeyField :: dataFields
  override def parentFields: List[FieldType] = List()
  override def prettyPrint(i: String) = s"${i}SameId($tableName, id=${primaryKeyField.name}, $fieldsPrettyString)${childrenPrettyString(i)}"
}