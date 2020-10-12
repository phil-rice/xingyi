package one.xingyi.core.orm

import java.io.OutputStream

import one.xingyi.core.optics.Lens
import one.xingyi.core.orm.BulkDataPointer.{pointerToNthChildL, pointerToNthL}

import scala.language.higherKinds


trait OrmBulkData[E] {
  def ormEntity: E with OrmEntity
  def tableName: TableName = ormEntity.tableName
  def data: List[List[Any]] = tableNameToData(tableName.tableName)
  def tableNameToData: Map[String, List[List[Any]]]
  def children: List[ChildOrmBulkData[_]]
}

trait ChildOrmBulkData[E] extends OrmBulkData[E] {
  def idsForPrettyPrint(parentId: Any): String
  //  def mapOfTableNameToIndexForParentId(parentId: Any, parentRow: List[Any]): List[(String, Int)]
  def pointer(parentId: Any, n: Int): ChildBulkDataPointer
  def asNullBulkDataPointer: NullBulkDataPointer = NullBulkDataPointer(this, children.map(_.asNullBulkDataPointer))
}


object MainBulkData {
  def apply(main: MainEntity, data: Map[OrmEntity, List[List[Any]]]): MainBulkData = {
    val tableNameToData = data.map { case (k, v) => k.tableName.tableName -> v }
    MainBulkData(main, tableNameToData, main.children.map(makeChild(main, _, tableNameToData)))
  }
  def makeChild(parent: OrmEntity, child: ChildEntity, tableNameToData: Map[String, List[List[Any]]]): ChildOrmBulkData[_] = child match {
    case o: OneToManyEntity => OneToManyBulkData(parent, o, tableNameToData, child.children.map(makeChild(child, _, tableNameToData)))
    case m: ManyToOneEntity => ManyToOneBulkData(parent, m, tableNameToData, child.children.map(makeChild(child, _, tableNameToData)))
    case s: SameIdEntity => SameIdBulkData(s, tableNameToData, child.children.map(makeChild(child, _, tableNameToData)))
  }
}

case class MainBulkData(ormEntity: MainEntity, tableNameToData: Map[String, List[List[Any]]], children: List[ChildOrmBulkData[_]]) extends OrmBulkData[MainEntity] {
  def pointer(n: Int): MainBulkDataPointer = {
    val id = ormEntity.primaryKeyFieldsAndIndex.getKey(data(n))
    MainBulkDataPointer(n, this, children.map(_.pointer(id, 0)))
  }
}
case class OneToManyBulkData(parentEntity: OrmEntity, ormEntity: OneToManyEntity, tableNameToData: Map[String, List[List[Any]]], children: List[ChildOrmBulkData[_]]) extends ChildOrmBulkData[OneToManyEntity] {
  val parentIdToListOfIndexes: Map[Any, List[Int]] = data.zipWithIndex.map { case (row, i) => (ormEntity.parentIdsAndIndex.getKey(row), i) }.groupBy(_._1).map(kv => kv._1 -> kv._2.map(_._2))

  override def pointer(parentId: Any, n: Int): ChildBulkDataPointer = {
    parentIdToListOfIndexes.getOrElse(parentId, Nil) match {
      case list if n >= list.size => NullBulkDataPointer(this, children.map(_.asNullBulkDataPointer))
      case list =>
        val index = list(n)
        val row = data(index)
        val id = ormEntity.primaryKeyFieldsAndIndex.getKey(row)
        FoundChildBulkDataPointer(n, index, parentId, this, children.map(child => child.pointer(id, 0)))
    }
  }
  override def idsForPrettyPrint(parentId: Any): String = parentIdToListOfIndexes(parentId).mkString(",")
}
case class SameIdBulkData(ormEntity: SameIdEntity, tableNameToData: Map[String, List[List[Any]]], children: List[ChildOrmBulkData[_]]) extends ChildOrmBulkData[SameIdEntity] {
  val idToIndex: Map[Any, Int] = tableNameToData(ormEntity.tableName.tableName).zipWithIndex.map { case (row, i) => ormEntity.primaryKeyFieldsAndIndex.getKey(row) -> i }.toMap
  override def pointer(parentId: Any, n: Int): ChildBulkDataPointer = {
    require(n == 0, s"In SameIdBulkData and asked for a pointer with a non zero n ${n}")
    idToIndex.lift(parentId) match {
      case Some(index) => FoundChildBulkDataPointer(n, index, parentId, this, children.map(_.pointer(parentId, 0)))
      case _ => NullBulkDataPointer(this, children.map(_.asNullBulkDataPointer))
    }
  }
  override def idsForPrettyPrint(parentId: Any): String = idToIndex(parentId).toString
}
case class ManyToOneBulkData(parentEntity: OrmEntity, ormEntity: ManyToOneEntity, tableNameToData: Map[String, List[List[Any]]], children: List[ChildOrmBulkData[_]]) extends ChildOrmBulkData[ManyToOneEntity] {
  val idToIndex: Map[Any, Int] = tableNameToData(ormEntity.tableName.tableName).zipWithIndex.map { case (row, i) => ormEntity.primaryKeyFieldsAndIndex.getKey(row) -> i }.toMap
  val keysAndIndex = ormEntity.idInParent.toKeysAndIndex(parentEntity)
  override def pointer(parentId: Any, n: Int): ChildBulkDataPointer = {
    require(n == 0, s"In SameIdBulkData and asked for a pointer with a non zero n ${n}")
    idToIndex.get(parentId) match {
      case None => NullBulkDataPointer(this, children.map(_.asNullBulkDataPointer))
      case Some(index) =>
        val id = keysAndIndex.getKey(data(index))
        FoundChildBulkDataPointer(n, index, parentId, this, children.map(_.pointer(id, 0)))
    }
  }
  override def idsForPrettyPrint(parentId: Any): String = idToIndex(parentId).toString
}


object BulkDataPointer {
  val pointerToNthL: Lens[ChildBulkDataPointer, Int] = Lens(_.nth, (b, p) => b.updatePointer(p))
  def pointerToNthChildL(n: Int): Lens[ChildBulkDataPointer, ChildBulkDataPointer] = Lens(_.children(n), (b, child) => b.updateNthChild(n, child))
}

sealed trait BulkDataPointer {
  def prettyPrint(indent: String): String
  protected def prettyPrintChildren(indent: String) = if (children.isEmpty) "noChildren" else s"children=\n${children.map(_.prettyPrint(indent + "  ")).mkString("\n")}"
  def nth: Int
  def bulkData: OrmBulkData[_]
  def children: List[ChildBulkDataPointer]

  def currentRow: Option[List[Any]]
  def pointerOrException: Int = if (nth == -1) throw new RuntimeException(s"Cannot access bulk pointer for ${bulkData.tableName}") else nth
  def listOfTableNameToDataPoint: List[(String, BulkDataPointer)] = (bulkData.tableName.tableName -> this) :: children.flatMap(_.listOfTableNameToDataPoint)
}

trait ChildBulkDataPointer extends BulkDataPointer {
  def indexIntoBulkData: Int
  def updatePointer(p: Int): ChildBulkDataPointer
  def updateNthChild(n: Int, child: ChildBulkDataPointer): ChildBulkDataPointer

  override def currentRow: Option[List[Any]] = bulkData.data.lift(indexIntoBulkData)
  def lensToBulkDataPointerWithTable(name: TableName): Option[Lens[ChildBulkDataPointer, ChildBulkDataPointer]] =
    if (name == bulkData.tableName) Some(Lens.identity) else children.zipWithIndex.flatMap { case (child, i) => child.lensToBulkDataPointerWithTable(name).map(pointerToNthChildL(i) andThen _) }.headOption
}

case class FoundChildBulkDataPointer(nth: Int, indexIntoBulkData: Int, parentId: Any, bulkData: ChildOrmBulkData[_], children: List[ChildBulkDataPointer]) extends ChildBulkDataPointer {
  override def updatePointer(p: Int): ChildBulkDataPointer = bulkData.pointer(parentId, p)
  override def updateNthChild(n: Int, child: ChildBulkDataPointer): ChildBulkDataPointer = copy(children = children.updated(n, child))
  override def prettyPrint(indent: String): String = s"${indent}Found(n=$nth,index=$indexIntoBulkData,$parentId,row=${currentRow},bulkData=${bulkData.tableName.tableName}(${bulkData.idsForPrettyPrint(parentId)}),${prettyPrintChildren(indent)}"
}

case class NullBulkDataPointer(bulkData: ChildOrmBulkData[_], children: List[ChildBulkDataPointer]) extends ChildBulkDataPointer {
  override def nth: Int = -1
  override def indexIntoBulkData: Int = -1
  override def updatePointer(p: Int): ChildBulkDataPointer = this
  override def updateNthChild(n: Int, child: ChildBulkDataPointer): ChildBulkDataPointer = this
  override def currentRow: Option[List[Any]] = None
  /** Ths index into the data for the current item */
  override def prettyPrint(indent: String): String = s"${indent}Null()"
}

case class MainBulkDataPointer(nth: Int, bulkData: OrmBulkData[_], children: List[ChildBulkDataPointer]) extends BulkDataPointer {
  override def prettyPrint(indent: String): String = s"${indent}Found($nth, bulkData=${bulkData.tableName},row=${currentRow},${prettyPrintChildren(indent)}"
  def fail(msg: String) = throw new RuntimeException(s"$msg. Legal tableMames are ${map.keys.toList.sortBy(_.toString)}")
  lazy val map: Map[String, BulkDataPointer] = ((bulkData.tableName.tableName -> this) :: listOfTableNameToDataPoint).toMap

  def currentRow(tableName: TableName): Option[List[Any]] = map(tableName.tableName).currentRow
  override def currentRow: Option[List[Any]] = bulkData.data.lift(nth)

  def keyValues[Context, Schema[_], T](context: Context, schema: Schema[T])(implicit getKey: SchemaMapKey[Schema], toTableAndFieldTypes: ToTableAndFieldTypes[Context, Schema]): List[(String, T)] = {
    val key = getKey.childKey(schema)
    toTableAndFieldTypes(schema).flatMap {
      case t@TableAndFieldTypes(tableName, fieldTypes) =>
        val bulkData = map(tableName.tableName).bulkData
        currentRow(tableName).map(row => (key -> t.tx(context, bulkData.ormEntity, fieldTypes)(row)))
    }
  }

  def allPointers[Context](tableName: TableName): Seq[MainBulkDataPointer] = {
    val tableL = childWithTableLOrException(tableName)
    val childPointer = tableL(this)
    val childToPoints = tableL andThen pointerToNthL
    (childPointer, childPointer.bulkData) match {
      case (f: FoundChildBulkDataPointer, o: OneToManyBulkData) => (0 to o.parentIdToListOfIndexes(f.parentId).size - 1).map(i => childToPoints.set(this, i))
      case (f: NullBulkDataPointer, _) => List()
      case (s, p) => throw new RuntimeException(s"Cannot iterate over table $tableName. The bulkdata is of type ${s.getClass}")
    }
  }

  def pointerToNthChildForMainL(n: Int): Lens[MainBulkDataPointer, ChildBulkDataPointer] = Lens(_.children(n), (b, child) => b.copy(children = children.updated(n, child)))

  def childWithTableL(name: TableName): Option[Lens[MainBulkDataPointer, ChildBulkDataPointer]] =
    children.zipWithIndex.flatMap { case (child, i) => child.lensToBulkDataPointerWithTable(name).map(childLens => pointerToNthChildForMainL(i) andThen childLens) }.headOption

  def childWithTableLOrException(name: TableName): Lens[MainBulkDataPointer, ChildBulkDataPointer] =
    childWithTableL(name).getOrElse(fail(s"Cannot find the lens to the tablename ${name} "))
  /** Ths index into the data for the current item, can be -1 if not valid */
}

class WriteToJsonForSchema[Schema[_], Context](context: Context, stream: OutputStream)
                                              (implicit toKey: SchemaMapKey[Schema], toTableAndFieldTypes: ToTableAndFieldTypes[Context, Schema], jsonToStreamFor: JsonToStreamFor[Context, Schema]) {
  var printComma: Boolean = false
  def putKeyValue[T](main: MainBulkDataPointer, schema: Schema[T]) {
    putKeyColon(toKey.childKey(schema))
    putValue(main, schema)
  }
  private def putKeyColon[T](key: String) = {
    printCommaIfNeeded
    JsonToStream.putEscapedWithQuotes(key, stream);
    stream.write(':')
  }
  def printCommaIfNeeded = {
    if (printComma) stream.write(',')
    printComma = true
  }
  def putValue[T](main: MainBulkDataPointer, schema: Schema[T]) =
    main.keyValues(context, schema).headOption.foreach(kv => jsonToStreamFor.putToJson(context, schema).put(context, schema, kv._2, stream))

  def toJsonforSimples(main: MainBulkDataPointer, simple: List[Schema[_]]): Unit = simple.foreach(putKeyValue(main, _))

  def toJsonforLinks(main: MainBulkDataPointer, links: List[Schema[_]]): Unit = {
    if (links.nonEmpty) {
      JsonToStream.putEscapedWithQuotes(""""_links":{""", stream)
      links.foreach(putKeyValue(main, _))
      stream.write('}')
    }
  }

  def toJsonForSingleChildObjects(main: MainBulkDataPointer, singleChildObjects: List[PartitionedSchema[Schema]]): Unit =
    singleChildObjects.foreach { singleChild =>
      putKeyColon(singleChild.key)
      toJson(main, singleChild)
    }
  def toJsonForOneManyChildObjects(main: MainBulkDataPointer, tableName: TableName, manyChildObject: PartitionedSchema[Schema]): Unit = {
    putKeyColon(manyChildObject.key)
    val oldPrintComma = printComma
    printComma = false
    stream.write('[')
    main.allPointers(tableName).foreach { child =>
      printCommaIfNeeded
      //      stream.write('<')
      toJson(child, manyChildObject)
      //      stream.write('>')
    }
    stream.write(']')
    printComma = oldPrintComma
  }
  def toJsonForManyChildObjects(main: MainBulkDataPointer, manyChildObjects: List[(TableName, PartitionedSchema[Schema])]): Unit =
    manyChildObjects.foreach { case (tableName, singleChild) => toJsonForOneManyChildObjects(main, tableName, singleChild) }

  def toJson(main: MainBulkDataPointer, schema: PartitionedSchema[Schema]): Unit = {
    stream.write('{')
    val oldPrintComma = printComma
    printComma = false
    toJsonforLinks(main, schema.links)
    toJsonForSingleChildObjects(main, schema.singleChildObjects)
    toJsonForManyChildObjects(main, schema.manyChildObjects)
    toJsonforSimples(main, schema.simple)
    stream.write('}')
    printComma = oldPrintComma
  }

}

object PartitionedSchema {
  def apply[Schema[_]](key: String, s: Schema[_])
                      (implicit mapKey: SchemaMapKey[Schema], isLinkFieldFilter: IsLinkFieldFilter[Schema],
                       isSimpleFieldFilter: IsSimpleFieldFilter[Schema], isObjectFieldFilter: IsObjectFieldFilter[Schema],
                       tableNameForManySchema: TableNameForManySchema[Schema]): PartitionedSchema[Schema] = {
    def isSimpleObject[T](c: Schema[T]) = mapKey.children(c).arity == OneChild
    def complexObject[T](c: Schema[T]) = tableNameForManySchema(c).map((_, apply(mapKey.childKey(c), c))).toList

    val children = mapKey.children(s).children
    PartitionedSchema[Schema](key,
      links = children.filter(isLinkFieldFilter(_)),
      simple = children.filter(isSimpleFieldFilter(_)),
      singleChildObjects = children.filter(isSimpleObject(_)).map(c => apply(mapKey.childKey(c), c)),
      manyChildObjects = children.flatMap(complexObject(_)))

  }
}
case class PartitionedSchema[Schema[_]](key: String,
                                        links: List[Schema[_]],
                                        simple: List[Schema[_]],
                                        singleChildObjects: List[PartitionedSchema[Schema]],
                                        manyChildObjects: List[(TableName, PartitionedSchema[Schema])]) {


}

