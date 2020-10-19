package one.xingyi.core.orm

import one.xingyi.core.optics.Lens
import one.xingyi.core.orm.BulkDataPointer.{pointerToNthChildL, pointerToNthL}

import scala.language.higherKinds

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
  def pointerOrException: Int = if (nth == -1) throw new RuntimeException(s"Cannot access bulk pointer for ${bulkData.alias}") else nth
  def listOfAliasToDataPoint: List[(String, BulkDataPointer)] = (bulkData.alias.tableName.tableName -> this) :: children.flatMap(_.listOfAliasToDataPoint)
}


trait ChildBulkDataPointer extends BulkDataPointer {
  def indexIntoBulkData: Int
  def updatePointer(p: Int): ChildBulkDataPointer
  def updateNthChild(n: Int, child: ChildBulkDataPointer): ChildBulkDataPointer

  override def currentRow: Option[List[Any]] = bulkData.data.lift(indexIntoBulkData)
  def lensToBulkDataPointerWithTable(alias: Alias): Option[Lens[ChildBulkDataPointer, ChildBulkDataPointer]] =
    if (alias == bulkData.alias) Some(Lens.identity) else children.zipWithIndex.flatMap { case (child, i) => child.lensToBulkDataPointerWithTable(alias).map(pointerToNthChildL(i) andThen _) }.headOption
}

case class FoundChildBulkDataPointer(nth: Int, indexIntoBulkData: Int, parentId: Any, bulkData: ChildOrmBulkData[_], children: List[ChildBulkDataPointer]) extends ChildBulkDataPointer {
  override def updatePointer(p: Int): ChildBulkDataPointer = bulkData.pointer(indexIntoBulkData, parentId, p)
  override def updateNthChild(n: Int, child: ChildBulkDataPointer): ChildBulkDataPointer = copy(children = children.updated(n, child))
  override def prettyPrint(indent: String): String =
    s"${indent}Found(n=$nth,index=$indexIntoBulkData,parentId=$parentId,row=${currentRow},bulkData=${bulkData.alias.prettyPrint}(${bulkData.idsForPrettyPrint(indexIntoBulkData, parentId)}),${prettyPrintChildren(indent)}"
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
  override def prettyPrint(indent: String): String = s"${indent}Found(nth=$nth, bulkData=${bulkData.alias.prettyPrint},row=${currentRow},${prettyPrintChildren(indent)}"
  def fail(msg: String) = throw new RuntimeException(s"$msg. Legal tableMames are ${map.keys.toList.sortBy(_.toString)}")
  lazy val map: Map[String, BulkDataPointer] = ((bulkData.alias.tableName.tableName -> this) :: listOfAliasToDataPoint).toMap

  def currentRow(alias: Alias): Option[List[Any]] = map(alias.tableName.tableName).currentRow
  override def currentRow: Option[List[Any]] = bulkData.data.lift(nth)

  def keyValues[Context: ZerothValueFromContext, Schema[_] : GetPattern, T](context: Context, schema: Schema[T])(implicit getKey: SchemaMapKey[Schema], toTableAndFieldTypes: ToAliasAndFieldTypes[Schema]): List[(String, T)] = {
    val key = getKey.childKey(schema)
    try {
      toTableAndFieldTypes(schema).flatMap {
        case t@AliasAndFieldTypes(alias, fieldTypes) =>
          map.get(alias.tableName.tableName) match {
            case Some(pointer) => try {
              val bulkData = pointer.bulkData
              currentRow(alias).map(row => (key -> t.tx(context, schema, bulkData.ormEntity, fieldTypes).apply(row)))
            } catch {case e: Exception => throw new RuntimeException(s"Error in keyValues($context,$key, $t", e)}
            case None => Nil
          }
      }
    }
    catch {case e: Exception => throw new RuntimeException(s"Error in keyValues($context,$key", e)}
  }

  def allPointers[Context](alias: Alias): Seq[MainBulkDataPointer] = {
    val tableL = childWithTableLOrException(alias)
    val childPointer = tableL(this)
    val childToPoints = tableL andThen pointerToNthL
    (childPointer, childPointer.bulkData) match {
      case (f: FoundChildBulkDataPointer, o: OneToManyBulkData) => (0 to o.parentIdToListOfIndexes(f.parentId).size - 1).map(i => childToPoints.set(this, i))
      case (f: NullBulkDataPointer, _) => List()
      case (s, p) => throw new RuntimeException(s"Cannot iterate over table $alias. The bulkdata is of type ${s.getClass}")
    }
  }

  def pointerToNthChildForMainL(n: Int): Lens[MainBulkDataPointer, ChildBulkDataPointer] = Lens(_.children(n), (b, child) => b.copy(children = children.updated(n, child)))

  def childWithTableL(alias: Alias): Option[Lens[MainBulkDataPointer, ChildBulkDataPointer]] =
    children.zipWithIndex.flatMap { case (child, i) => child.lensToBulkDataPointerWithTable(alias).map(childLens => pointerToNthChildForMainL(i) andThen childLens) }.headOption

  def childWithTableLOrException(alias: Alias): Lens[MainBulkDataPointer, ChildBulkDataPointer] =
    childWithTableL(alias).getOrElse(fail(s"Cannot find the lens to the tablename ${alias} "))
}
