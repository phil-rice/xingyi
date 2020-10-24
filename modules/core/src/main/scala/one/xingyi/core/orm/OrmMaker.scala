package one.xingyi.core.orm

import java.io.ByteArrayOutputStream

import one.xingyi.core.accessors.HasChildrenF
import one.xingyi.core.json.{JsonObject, JsonParser, JsonValue}
import one.xingyi.core.map.Maps.MapOfListsOps

import scala.language.higherKinds

trait OrmMaker[T] extends (MainEntity => Map[OrmEntity, List[List[AnyRef]]] => Stream[T])

object OrmMaker {
  def prettyPrintData(data: Map[OrmEntity, List[List[AnyRef]]]) = {
    data.flatMap { case (entity, lists) =>
      s"${entity.tableName.tableName}(size=${lists.size})" :: lists.map { oneRow =>
        entity.fieldsForCreate.zipAll(oneRow, FieldType("missingName"), "missingValues").zipWithIndex.
          map { case ((f, d), i) => s"  $i ${f.name}=$d" }.mkString(",")
      }
    }
  }
  def apply[Context: LinkPrefixFrom, Schema[_] : SchemaMapKey : IsLinkFieldFilter : HasChildrenF : ArrayAlias : GetPattern]
  (context: Context, schema: Schema[_])
  (implicit toTableAndFieldTypes: ToAliasAndFieldTypes[Schema], jsonToStreamFor: JsonToStreamFor[Schema]): OrmMaker[String] = new OrmMaker[String] {
    override def apply(main: MainEntity): Map[OrmEntity, List[List[Any]]] => Stream[String] = { entityToData =>
      val mainBulkData = MainBulkData(main, entityToData)
      entityToData(main).indices.map { i =>
        val stream = new ByteArrayOutputStream()
        val writer = new WriteToJsonForSchema[Schema, Context](context, stream)
        writer.toJson(mainBulkData.pointer(i), PartitionedSchema("notUsed", schema))
        stream.toString
      }.toStream
    }
  }


  type RawOrmMap = Map[OrmEntity, List[List[AnyRef]]]
  type MakeTFn[T] = OrmEntity => List[Any] => T

  def toMapOfIdToList(map: RawOrmMap): Map[OrmEntity, Map[Any, List[AnyRef]]] =
    map.mapValues {
      _.map {
        case head :: rest => head -> rest;
        case _ => throw new RuntimeException()
      }.toMap[Any, List[AnyRef]]
    }.toMap

  def toListofIdAnd[T](map: RawOrmMap, entity: OrmEntity)(fn: MakeTFn[T]): List[(Any, T)] =
    map(entity).map {
      case id :: rest => id -> fn(entity)(rest);
      case _ => throw new RuntimeException()
    }

  def toMapofIdTo[T](map: RawOrmMap, entity: OrmEntity)(fn: MakeTFn[T]): Map[Any, T] =
    toListofIdAnd(map, entity)(fn).toMap


  def toJson[J: JsonParser](entity: OrmEntity)(list: List[Any]): JsonObject = {
    //    println(s"OrmMaker.toJson $list  rows to drop ${entity.colsToDropForValues}")
    def fromTuple[T](data: Any, fieldType: FieldType[T]): (String, JsonValue) = fieldType.name -> fieldType.writeToJson(data.asInstanceOf[T])
    JsonObject(list.zip(entity.dataFields).map {
      case (data, df) => fromTuple(data, df)
    }: _*)
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