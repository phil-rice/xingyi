package one.xingyi.core.orm

import java.io.OutputStream

import scala.language.higherKinds

/** OrmBuildData is 'the data we have loaded about an entity'. many of the implemtors have a 'pointer' which points to
 * the 'current item being pulled in'. We use the pointers to 'walkthrough the data',
 *
 *
 * When we pull an object graph into memory, we pull it into a 'tableNameToData' and then rip it apart into OrmBuildData.
 * So here:
 * <ul><li>ormEntity/Alias define the entity
 * <li>data is the data about this entity
 * <li>children is about how this relates to other nodes in the object graph
 * </ul>
 *
 * *
 */
trait OrmBulkData[E] {
  def ormEntity: E with OrmEntity
  def alias: Alias = ormEntity.alias
  def data: List[List[Any]] = tableNameToData(alias.table.name)
  def tableNameToData: Map[String, List[List[Any]]]
  def children: List[ChildOrmBulkData[_]]
  def prettyPrint(map: Map[OrmEntity, List[List[Any]]]): List[String] =
    s"${ormEntity.alias.prettyPrint}" :: map(ormEntity).map(l => "  " + l) ::: children.flatMap(_.prettyPrint(map))
}


/** This is the root interface for BulkData that are children.
 *
 * The most important method is  'pointer' which 'given a parent id' return a pointer to the data in this about that parent.
 * The pointers are mutable
 * */
trait ChildOrmBulkData[E] extends OrmBulkData[E] {
  def idsForPrettyPrint(parentIndex: Int, parentId: Any): String
  //  def mapOfTableNameToIndexForParentId(parentId: Any, parentRow: List[Any]): List[(String, Int)]
  def pointer(parentIndex: Int, parentId: Any, n: Int): ChildBulkDataPointer
  def asNullBulkDataPointer: NullBulkDataPointer = NullBulkDataPointer(this, children.map(_.asNullBulkDataPointer))
}

object MainBulkData {
  def apply(main: MainEntity, data: Map[OrmEntity, List[List[Any]]]): MainBulkData = {
    val tableNameToData = data.map { case (k, v) => k.tableName.name -> v }
    MainBulkData(main, tableNameToData, main.children.map(makeChild(main, _, tableNameToData)))
  }
  def makeChild(parent: OrmEntity, child: ChildEntity, tableNameToData: Map[String, List[List[Any]]]): ChildOrmBulkData[_] = child match {
    case o: OneToManyEntity => OneToManyBulkData(parent, o, tableNameToData, child.children.map(makeChild(child, _, tableNameToData)))
    case m: ManyToOneEntity => ManyToOneBulkData(parent, m, tableNameToData, child.children.map(makeChild(child, _, tableNameToData)))
    case s: SameIdEntity => SameIdBulkData(s, tableNameToData, child.children.map(makeChild(child, _, tableNameToData)))
  }
}

/** This is a BulkData, and it's defining characteristic is that it isn't the child of any other BulkData.
 *
 * it's pointer method points to the 'nth' item in the main bulk. When we are iterating over the data we use the pointer to do that */
case class MainBulkData(ormEntity: MainEntity, tableNameToData: Map[String, List[List[Any]]], children: List[ChildOrmBulkData[_]]) extends OrmBulkData[MainEntity] {
  def pointer(n: Int): MainBulkDataPointer = {
    val id = ormEntity.primaryKeyFieldsAndIndex.getKey(data(n))
    MainBulkDataPointer(n, this, children.map(_.pointer(n, id, 0)))
  }

}
/** As the name suggests this is a Bulk data that for every parent has zero or more child items.
 *
 * We know that the parent id is stored in this data: which you can see looking at the pointer method */
case class OneToManyBulkData(parentEntity: OrmEntity, ormEntity: OneToManyEntity, tableNameToData: Map[String, List[List[Any]]], children: List[ChildOrmBulkData[_]]) extends ChildOrmBulkData[OneToManyEntity] {
  val parentIdToListOfIndexes: Map[Any, List[Int]] = data.zipWithIndex.map { case (row, i) => (ormEntity.parentIdsAndIndex.getKey(row), i) }.groupBy(_._1).map(kv => kv._1 -> kv._2.map(_._2))

  override def pointer(parentIndex: Int, parentId: Any, n: Int): ChildBulkDataPointer = {
    parentIdToListOfIndexes.getOrElse(parentId, Nil) match {
      case list if n >= list.size => NullBulkDataPointer(this, children.map(_.asNullBulkDataPointer))
      case list =>
        val index = list(n)
        val row = data(index)
        val id = ormEntity.primaryKeyFieldsAndIndex.getKey(row)
        FoundChildBulkDataPointer(n, index, parentId, this, children.map(child => child.pointer(index, id, 0)))
    }
  }
  override def idsForPrettyPrint(parentIndex: Int, parentId: Any): String = parentIdToListOfIndexes(parentId).mkString(",")
}

/** As the name suggests this is a Bulk data for a table that shares the same primary key as the parent. Thus there will be zero or one items for every parent */
case class SameIdBulkData(ormEntity: SameIdEntity, tableNameToData: Map[String, List[List[Any]]], children: List[ChildOrmBulkData[_]]) extends ChildOrmBulkData[SameIdEntity] {
  val idToIndex: Map[Any, Int] = tableNameToData(ormEntity.alias.table.name).zipWithIndex.map { case (row, i) => ormEntity.primaryKeyFieldsAndIndex.getKey(row) -> i }.toMap
  override def pointer(parentIndex: Int, parentId: Any, n: Int): ChildBulkDataPointer = {
    require(n == 0, s"In SameIdBulkData and asked for a pointer with a non zero n ${n}")
    idToIndex.lift(parentId) match {
      case Some(index) => FoundChildBulkDataPointer(n, index, parentId, this, children.map(_.pointer(index, parentId, 0)))
      case _ => NullBulkDataPointer(this, children.map(_.asNullBulkDataPointer))
    }
  }
  override def idsForPrettyPrint(parentIndex: Int, parentId: Any): String = idToIndex(parentId).toString
}

/** As the name suggests this is a Bulk data for a table in which the childId is stored in the parent. Thus there will be zero or one items for every parent. There may well be one or multiple parents that
 * share the child */
case class ManyToOneBulkData(parentEntity: OrmEntity, ormEntity: ManyToOneEntity, tableNameToData: Map[String, List[List[Any]]], children: List[ChildOrmBulkData[_]]) extends ChildOrmBulkData[ManyToOneEntity] {
  val idToIndex: Map[Any, Int] = tableNameToData(ormEntity.alias.table.name).zipWithIndex.map { case (row, i) => ormEntity.primaryKeyFieldsAndIndex.getKey(row) -> i }.toMap
  val keysAndIndex = ormEntity.idInParent.toKeysAndIndex(parentEntity)
  override def pointer(parentIndex: Int, parentId: Any, n: Int): ChildBulkDataPointer = {
    require(n == 0, s"In SameIdBulkData and asked for a pointer with a non zero n ${n}")
    val id: Any = myId(parentIndex)
    idToIndex.get(id) match {
      case None => NullBulkDataPointer(this, children.map(_.asNullBulkDataPointer))
      case Some(index) => FoundChildBulkDataPointer(n, index, parentId, this, children.map(_.pointer(index, id, 0)))
    }
  }
  def myId(parentIndex: Int): Any = {
    val parentRow = tableNameToData(parentEntity.tableName.name)(parentIndex)
    keysAndIndex.getKey(parentRow)
  }
  override def idsForPrettyPrint(parentIndex: Int, parentId: Any): String = idToIndex.get(myId(parentIndex)).toString
}

class WriteToJsonForSchema[Schema[_] : GetPattern, Context: LinkPrefixFrom](context: Context, stream: OutputStream)
                                                                           (implicit toKey: SchemaMapKey[Schema], toTableAndFieldTypes: ToAliasAndFieldTypes[Schema], jsonToStreamFor: JsonToStreamFor[Schema]) {
  var printComma: Boolean = false
  private def putKeyColon[T](key: String) = {
    printCommaIfNeeded
    JsonToStream.putEscapedWithQuotes(key, stream);
    stream.write(':')
  }
  def printCommaIfNeeded = {
    if (printComma) stream.write(',')
    printComma = true
  }
  def putKeyValue[T](main: MainBulkDataPointer, schema: Schema[T]) =
    main.keyValues(context, schema).headOption match {
      case Some(kv) =>
        putKeyColon(toKey.childKey(schema))
        jsonToStreamFor.putToJson(schema).put(schema, kv._2, stream)
      case _ => ;
    }


  def toJsonforSimples(main: MainBulkDataPointer, simple: List[Schema[_]]): Unit = simple.foreach(putKeyValue(main, _))

  def toJsonforLinks(main: MainBulkDataPointer, links: List[Schema[_]]): Unit = {
    if (links.nonEmpty) {
      JsonToStream.putUnescaped(stream, """"_links":{""")
      links.foreach(putKeyValue(main, _))
      stream.write('}')
      printComma = true
    }
  }

  def toJsonForSingleChildObjects(main: MainBulkDataPointer, singleChildObjects: List[PartitionedSchema[Schema]]): Unit =
    singleChildObjects.foreach { singleChild =>
      putKeyColon(singleChild.key)
      toJson(main, singleChild)
    }
  def toJsonForOneManyChildObjects(main: MainBulkDataPointer, alias: Alias, manyChildObject: PartitionedSchema[Schema]): Unit = {
    putKeyColon(manyChildObject.key)
    val oldPrintComma = printComma
    printComma = false
    stream.write('[')
    main.allPointers(alias).foreach { child =>
      printCommaIfNeeded
      toJson(child, manyChildObject)
    }
    stream.write(']')
    printComma = oldPrintComma
  }
  def toJsonForManyChildObjects(main: MainBulkDataPointer, manyChildObjects: List[(Alias, PartitionedSchema[Schema])]): Unit =
    manyChildObjects.foreach { case (alias, singleChild) => toJsonForOneManyChildObjects(main, alias, singleChild) }

  def toJson(main: MainBulkDataPointer, schema: PartitionedSchema[Schema]): Unit = {
    stream.write('{')
    val oldPrintComma = printComma
    printComma = false
    toJsonforLinks(main, schema.links)
    toJsonForSingleChildObjects(main, schema.objects)
    toJsonForManyChildObjects(main, schema.arrays)
    toJsonforSimples(main, schema.simple)
    stream.write('}')
    printComma = oldPrintComma
  }

}

object PartitionedSchema {
  def apply[Schema[_]](key: String, s: Schema[_])
                      (implicit mapKey: SchemaMapKey[Schema],
                       isLinkFieldFilter: IsLinkFieldFilter[Schema],
                       isSimpleFieldFilter: IsSimpleFieldFilter[Schema],
                       isObjectFieldFilter: IsObjectFieldFilter[Schema],
                       arrayTableName: ArrayAlias[Schema]): PartitionedSchema[Schema] = {

    val children = mapKey.children(s)
    PartitionedSchema[Schema](key,
      links = children.filter(isLinkFieldFilter(_)),
      simple = children.filter(isSimpleFieldFilter(_)),
      objects = children.collect { case c if isObjectFieldFilter(c) => apply(mapKey.childKey(c), c) },
      arrays = children.collect { case c if arrayTableName(c).nonEmpty => arrayTableName(c).get -> apply(mapKey.childKey(c), c) })
  }
}
case class PartitionedSchema[Schema[_]](key: String,
                                        links: List[Schema[_]],
                                        simple: List[Schema[_]],
                                        objects: List[PartitionedSchema[Schema]],
                                        arrays: List[(Alias, PartitionedSchema[Schema])]) {


}

