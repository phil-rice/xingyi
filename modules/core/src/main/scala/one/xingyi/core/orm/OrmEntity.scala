/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.orm

import one.xingyi.core.json.{JsonObject, JsonParser, JsonValue}
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


  def toJson[J: JsonParser](entity: OrmEntity)(list: List[Any]): JsonObject = {
    //    println(s"OrmMaker.toJson $list  rows to drop ${entity.colsToDropForValues}")
    def fromTuple[T](data: Any, fieldType: FieldType[T]): (String, JsonValue) = fieldType.name -> fieldType.writeToJson(data.asInstanceOf[T])
    JsonObject(list.drop(entity.colsToDropForValues).zip(entity.dataFields).map { case (data, df) => fromTuple(data, df) }: _*)
  }

  def toIdAndJson[J: JsonParser](entity: OrmEntity)(list: List[AnyRef]): (String, JsonObject) = list match {
    case primaryKey :: (rest: List[AnyRef]) => (primaryKey.toString, toJson(entity)(list))
    case _ => throw new RuntimeException("Cannot convert toIdAndJson")
  }

  def toJson[J: JsonParser](entity: OrmEntity, map: Map[OrmEntity, List[List[AnyRef]]]): List[(String, JsonObject)] = {
    val dataJsonObject: List[(String, JsonObject)] = map(entity).map(toIdAndJson[J](entity))
    val childJsonObjects: List[(String, JsonObject)] = entity.children.map(child => child.tableName -> JsonObject(toJson(child, map): _*))
    dataJsonObject ::: childJsonObjects

  }


  implicit def OrmMakerForJson[J: JsonParser]: OrmMaker[JsonObject] = new OrmMaker[JsonObject] {
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
  def primaryKeyField: FieldType[_]
  def dataFields: List[FieldType[_]]
  def children: List[ChildEntity]
  def fieldsForCreate: List[FieldType[_]]
  def fieldsAddedByChildren: List[FieldType[_]] = children.flatMap(_.parentFields)
  def colsToDropForValues = fieldsForCreate.size - dataFields.size
  def nonDataFields = fieldsForCreate.take(colsToDropForValues)
  protected def childrenPrettyString(indent: String) = if (children.isEmpty) "" else children.map(_.prettyPrint(indent + "  ")).mkString("{\n", "\n", s"$indent\n}")
  protected def fieldsPrettyString = {
    val data = s"data=${dataFields.map(_.name).mkString(",")}"
    val addedByChildren = if (fieldsAddedByChildren.isEmpty) "" else s"childrenAdded=${fieldsAddedByChildren.map(_.name).mkString(",")}, "
    addedByChildren + data
  }
  def prettyPrint(indent: String): String

  def dropTable(implicit fastOrmSql: FastOrmSql) = fastOrmSql.dropTable(this)
  def createTable(implicit fastOrmSql: FastOrmSql) = fastOrmSql.createTable(this)
  def dropTempTable(implicit fastOrmSql: FastOrmSql) = fastOrmSql.dropTempTable(this)
  def drainSql(implicit fastOrmSql: FastOrmSql): String = fastOrmSql.drainSql(this)
  def insertSql(implicit fastOrmSql: FastOrmSql): String = fastOrmSql.drainSql(this)

}

sealed trait ChildEntity extends OrmEntity {
  def createTempTable(implicit fastOrmSql: FastOrmSql): OrmEntity => String
  def parentFields: List[FieldType[_]]
}


case class MainEntity(tableName: String, alias: String, primaryKeyField: FieldType[_], dataFields: List[FieldType[_]], children: List[ChildEntity]) extends OrmEntity {
  val fieldsForCreate = primaryKeyField :: fieldsAddedByChildren ::: dataFields

  def stream[T](batchConfig: OrmBatchConfig)(implicit ormMaker: OrmMaker[T], fastReaderOps: FastReaderDal, sqlOps: FastOrmSql): Stream[T] = stream(FastReader(batchConfig), 0)
  private def stream[T](fastReader: FastReader[T], n: Int): Stream[T] = {
    val subStream: Stream[T] = fastReader(this)(n)
    if (subStream.isEmpty) subStream else subStream #::: stream(fastReader, n + 1)
  }
  override def prettyPrint(i: String) = s"${i}MainEntity($tableName, id=${primaryKeyField.name}, $fieldsPrettyString)${childrenPrettyString(i)}"
  def createTempTable(implicit fastOrmSql: FastOrmSql): BatchDetails => String = fastOrmSql.createMainTempTable(this)
}
case class OneToManyEntity(tableName: String, alias: String, primaryKeyField: FieldType[_], parentId: FieldType[_], dataFields: List[FieldType[_]], children: List[ChildEntity]) extends ChildEntity {
  val fieldsForCreate: List[FieldType[_]] = primaryKeyField :: parentId :: dataFields
  override def parentFields: List[FieldType[_]] = List()
  override def prettyPrint(i: String) = s"${i}OneToMany($tableName, id=${primaryKeyField.name}, parent=${parentId.name} $fieldsPrettyString)${childrenPrettyString(i)}"
  override def createTempTable(implicit fastOrmSql: FastOrmSql) = fastOrmSql.createOneToManyTempTable(this)
}
case class ManyToOneEntity(tableName: String, alias: String, primaryKeyField: FieldType[_], idInParent: FieldType[_], dataFields: List[FieldType[_]], children: List[ChildEntity]) extends ChildEntity {
  val fieldsForCreate = primaryKeyField :: dataFields
  override def parentFields: List[FieldType[_]] = List(idInParent)
  override def prettyPrint(i: String) = s"${i}ManyToOne($tableName, id=${primaryKeyField.name}, idInParent=${idInParent.name} $fieldsPrettyString)${childrenPrettyString(i)}"
  override def createTempTable(implicit fastOrmSql: FastOrmSql): OrmEntity => String = fastOrmSql.createManyToOneTempTable(this)
}
case class SameIdEntity(tableName: String, alias: String, primaryKeyField: FieldType[_], dataFields: List[FieldType[_]], children: List[ChildEntity]) extends ChildEntity {
  override val fieldsForCreate = primaryKeyField :: dataFields
  override def parentFields: List[FieldType[_]] = List()
  override def prettyPrint(i: String) = s"${i}SameId($tableName, id=${primaryKeyField.name}, $fieldsPrettyString)${childrenPrettyString(i)}"
  override def createTempTable(implicit fastOrmSql: FastOrmSql): OrmEntity => String = fastOrmSql.createSameIdTempTable(this)
}