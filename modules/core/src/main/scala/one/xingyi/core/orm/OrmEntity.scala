/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.orm

import java.io

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
    JsonObject(list.zip(entity.dataFields).map { case (data, df) => fromTuple(data, df) }: _*)
  }

  def toIdAndJson[J: JsonParser](entity: OrmEntity)(list: List[AnyRef]): (String, JsonObject) = list match {
    case primaryKey :: (rest: List[AnyRef]) => (primaryKey.toString, toJson(entity)(list))
    case _ => throw new RuntimeException("Cannot convert toIdAndJson")
  }

  def toJson[J: JsonParser](entity: OrmEntity, map: Map[OrmEntity, List[List[AnyRef]]]): List[(String, JsonObject)] = {
    val dataJsonObject: List[(String, JsonObject)] = map(entity).map(toIdAndJson[J](entity))
    val childJsonObjects: List[(String, JsonObject)] = entity.children.map(child => child.tableName.tableName -> JsonObject(toJson(child, map): _*))
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
  def str(n: Int)(implicit list: List[Any]): String = list(n).toString

}

trait OrmEntity {
  def tableName: TableName
  def alias: String
  def primaryKeyField: Keys
  def primaryKeyFieldsAndIndex: KeysAndIndex = primaryKeyField.toKeysAndIndex(this)
  def dataFields: List[FieldType[_]]
  def children: List[ChildEntity]
  def fieldsForCreate: List[FieldType[_]] = (dataFields ::: fieldsAddedByChildren ::: primaryKeyField.list).distinct

  def fieldsAddedByChildren: List[FieldType[_]] = children.flatMap(_.parentFields)
  protected def childrenPrettyString(indent: String) = if (children.isEmpty) "" else children.map(_.prettyPrint(indent + "  ")).mkString("{\n", "\n", s"\n$indent}")
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
trait SingleChild extends ChildEntity {
  def getData[X](parent: OrmEntity, childsData: Map[Any, X], parentsData: List[Any], default: (KeysAndIndex, Map[Any, X], Any) => X = Keys.notFound _): X

  def toMap[X](data: Map[OrmEntity, List[List[Any]]], fn: List[Any] => X): Map[Any, X] =
    data(this).foldLeft[Map[Any, X]](Map())((acc, list) => primaryKeyFieldsAndIndex.asPrimaryKeyAddTo(acc, list, fn(list)))


}

object ChildEntity {
}

case class Keys(val list: List[FieldType[_]]) {
  val nameString: String = list.map(_.name).mkString(",")
  def checkCanLinkTo(keys: Keys) {
    require(keys.list.size == list.size, s"Cannot link keys with $nameString to keys with ${keys.nameString} as size mismatch")
    list.zip(keys.list).foreach { case (k1, k2) => if (k1.classTag.runtimeClass != k2.classTag.runtimeClass)
      throw new RuntimeException(s"Validation. Cannot use $nameString to link to ${keys.nameString}. Fields $k1 and $k2 have different types")
    }
  }
  var map = Map[OrmEntity, KeysAndIndex]()
  def addToMap(entity: OrmEntity) = {
    val result: KeysAndIndex = KeysAndIndex(list.map(l => (entity.fieldsForCreate.indexOf(l), l)))
    map = map + (entity -> result)
    result
  }
  def toKeysAndIndex(entity: OrmEntity): KeysAndIndex = map.getOrElse(entity, addToMap(entity))
}

object Keys {
  def notFound[V](keys: KeysAndIndex, map: Map[Any, V], key: Any) = throw new RuntimeException(s"Cannot find key $key when the keys are $keys map is $map")
  def apply(str: String): Keys = Keys((str.split(",").map(_.trim).map(FieldType.apply)).toList)

  def zip(keys1: Keys, keys2: Keys): List[(FieldType[_], FieldType[_])] = {
    keys1.checkCanLinkTo(keys2)
    keys1.list.zip(keys2.list)
  }
}

case class KeysAndIndex(list: List[(Int, FieldType[_])]) {
  val indexes = list.map(_._1)
  def prettyPrint = s"KeysAndIndex(${list.map { case (i, f) => s"$i,${f.name}" }.mkString(",")})"
  def getKey(oneRow: List[Any]): Any = if (indexes.size == 0) oneRow(indexes.head) else indexes.map(oneRow)
  def asPrimaryKeyAddTo[V](map: Map[Any, V], oneRow: List[Any], data: V): Map[Any, V] = map + (getKey(oneRow) -> data)
  def getFrom[V](map: Map[Any, V], oneRow: List[Any], default: (KeysAndIndex, Map[Any, V], Any) => V): V = {
    val key = getKey(oneRow)
    map.getOrElse(key, default(this, map, key))
  }

}

case class MainEntity(tableName: TableName, alias: String, primaryKeyField: Keys, dataFields: List[FieldType[_]], children: List[ChildEntity]) extends OrmEntity {
  def stream[T](batchConfig: OrmBatchConfig)(implicit ormMaker: OrmMaker[T], fastReaderOps: FastReaderDal, sqlOps: FastOrmSql): Stream[T] = stream(FastReader(batchConfig), 0)
  private def stream[T](fastReader: FastReader[T], n: Int): Stream[T] = {
    val subStream: Stream[T] = fastReader(this)(n)
    if (subStream.isEmpty) subStream else subStream #::: stream(fastReader, n + 1)
  }
  override def prettyPrint(i: String) = s"${i}MainEntity(${tableName.tableName}, id=${primaryKeyFieldsAndIndex.prettyPrint}, $fieldsPrettyString)${childrenPrettyString(i)}"
  def createTempTable(implicit fastOrmSql: FastOrmSql): BatchDetails => String = fastOrmSql.createMainTempTable(this)
}
case class OneToManyEntity(tableName: TableName, alias: String, primaryKeyField: Keys, parentId: Keys, dataFields: List[FieldType[_]], children: List[ChildEntity]) extends ChildEntity {
  override val fieldsForCreate: List[FieldType[_]] = (super.fieldsForCreate ::: parentId.list).distinct
  val parentIdsAndIndex = parentId.toKeysAndIndex(this)
  override def parentFields: List[FieldType[_]] = List()
  override def prettyPrint(i: String) = s"${i}OneToMany(${tableName.tableName}, id=${primaryKeyFieldsAndIndex.prettyPrint}, parent=${parentId.nameString} $fieldsPrettyString)${childrenPrettyString(i)}"
  override def createTempTable(implicit fastOrmSql: FastOrmSql) = fastOrmSql.createOneToManyTempTable(this)
  def toOneToManyMap[X](data: Map[OrmEntity, List[List[Any]]], parent: OrmEntity, fn: List[Any] => X): Map[Any, List[X]] =
    data(this).groupBy(parentIdsAndIndex.getKey).map { t => (t._1, t._2.map(fn)) }
  protected def emptyList[X](keys: KeysAndIndex, map: Map[Any, List[X]], key: Any) = Nil
  def getData[X](parent: OrmEntity, childsData: Map[Any, List[X]], parentsData: List[Any]): List[X] =
    parent.primaryKeyFieldsAndIndex.getFrom(childsData, parentsData, emptyList)
}

/** This will have zero or one entries for each item in the parent. It will be in 'step' with it... allowing cursors to advance together */
case class OneToZeroOneEntity(tableName: TableName, alias: String, primaryKeyField: Keys, idInParent: Keys, dataFields: List[FieldType[_]], children: List[ChildEntity]) extends SingleChild {
  override def parentFields: List[FieldType[_]] = idInParent.list
  override def prettyPrint(i: String) = s"${i}ManyToOne(${tableName.tableName}, id=${primaryKeyField.nameString}, idInParent=${idInParent.nameString} $fieldsPrettyString)${childrenPrettyString(i)}"
  override def createTempTable(implicit fastOrmSql: FastOrmSql): OrmEntity => String = fastOrmSql.createOneToZeroOneEntityTempTable(this)
  def getData[X](parent: OrmEntity, childsData: Map[Any, X], parentsData: List[Any], default: (KeysAndIndex, Map[Any, X], Any) => X = Keys.notFound): X = {
    val idsInParentAndIndex = idInParent.toKeysAndIndex(parent)
    idsInParentAndIndex.getFrom[X](childsData, parentsData, default)
  }
}
/** this is typically a look up reference. It is very similar to 'oneToZeroOneEntity' except that many of the parent are likely to share the same value. Thus it won't be in sync */
case class ManyToOneEntity(tableName: TableName, alias: String, primaryKeyField: Keys, idInParent: Keys, dataFields: List[FieldType[_]], children: List[ChildEntity]) extends SingleChild {
  override def parentFields: List[FieldType[_]] = idInParent.list
  override def prettyPrint(i: String) = s"${i}ManyToOne(${tableName.tableName}, id=${primaryKeyField.nameString}, idInParent=${idInParent.nameString} $fieldsPrettyString)${childrenPrettyString(i)}"
  override def createTempTable(implicit fastOrmSql: FastOrmSql): OrmEntity => String = fastOrmSql.createManyToOneTempTable(this)
  def getData[X](parent: OrmEntity, childsData: Map[Any, X], parentsData: List[Any], default: (KeysAndIndex, Map[Any, X], Any) => X = Keys.notFound): X = {
    val idsInParentAndIndex = idInParent.toKeysAndIndex(parent)
    idsInParentAndIndex.getFrom[X](childsData, parentsData, default)
  }
}
case class SameIdEntity(tableName: TableName, alias: String, primaryKeyField: Keys, dataFields: List[FieldType[_]], children: List[ChildEntity]) extends SingleChild {
  override def parentFields: List[FieldType[_]] = List()
  override def prettyPrint(i: String) = s"${i}SameId(${tableName.tableName}, id=${primaryKeyField.nameString}, $fieldsPrettyString)${childrenPrettyString(i)}"
  override def createTempTable(implicit fastOrmSql: FastOrmSql): OrmEntity => String = fastOrmSql.createSameIdTempTable(this)
  def getData[X](parent: OrmEntity, childsData: Map[Any, X], parentsData: List[Any], default: (KeysAndIndex, Map[Any, X], Any) => X = Keys.notFound): X =
    parent.primaryKeyFieldsAndIndex.getFrom[X](childsData, parentsData, default)
}