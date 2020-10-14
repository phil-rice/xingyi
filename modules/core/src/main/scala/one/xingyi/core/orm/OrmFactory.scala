package one.xingyi.core.orm

import scala.language.higherKinds
import scala.reflect.ClassTag


case class EntityAndPath[E <: OrmEntity](entity: E, paths: Array[List[Int]]) {
  require(entity.fieldsForCreate.size == paths.size, s"Should have same number of paths as have fieldsForCreate. In entity ${entity.fieldsForCreate.size} in paths: ${paths.size}")
}

case class FieldTypeAndIndex[T](fieldType: FieldType[T], index: Int)

trait OrmValueTransformer[T] extends ((Array[FieldTypeAndIndex[_]], Array[Any]) => T)

object OrmValueTransformer {
  def defaultOrmValueTransformer[T](fn: Any => T)(implicit classTag: ClassTag[T]): OrmValueTransformer[T] = new OrmValueTransformer[T] {
    override def apply(v1: Array[FieldTypeAndIndex[_]], v2: Array[Any]): T = {
      require(v1.size == 1, s"Cannot transform into a ${classTag.runtimeClass.getSimpleName} using defaultValueTransformer if more than one value in ${v1.toList}")
      v2(v1.head.index) match {
        case t: T => t
        case res => try {fn(res) } catch {
          case e: Exception =>
            throw new RuntimeException(s"expected a ${classTag.runtimeClass.getSimpleName} has a ${res.getClass.getSimpleName} which is [$res] for ${v1.head}", e)
        }
      }
    }
  }
  implicit val ormValueTransformerForString: OrmValueTransformer[String] = defaultOrmValueTransformer[String](_.toString)
  implicit val ormValueTransformerForInt = defaultOrmValueTransformer[Int] { case s: String if s == "" => 0; case s => s.toString.toInt }
  implicit val ormValueTransformerForDouble = defaultOrmValueTransformer[Double] { case s: String if s == "" => 0; case s => s.toString.toDouble }
  implicit val ormValueTransformerForPlaceHolder: OrmValueTransformer[Placeholder] = (v1: Array[FieldTypeAndIndex[_]], v2: Array[Any]) => throw new RuntimeException("Should not be called")
}

case class OrmValueGetter[T](tableName: TableName, fieldTypes: List[FieldType[_]])(implicit val tx: OrmValueTransformer[T]) {
  def ormValueGetterForARow(list: List[String]): OrmValueGetterForARow[T] = OrmValueGetterForARow(tableName, fieldTypes.map(_.withIndex(list)).toArray, tx)
}
case class OrmValueGetterForARow[T](tableName: TableName, fieldTypes: Array[FieldTypeAndIndex[_]], tx: OrmValueTransformer[T]) {
  def apply(oneRow: Array[Any]) = tx(fieldTypes, oneRow)
}


/** Asingle item in the schema might be in several places in the database (a key/a foreign key).
 * That item might be represented by multiple fields (e.g. a composite string, or a date where the date is stored in multiple fields
 *
 * here the list reflects the multiple places (keys/foreign keys) and the value getter itself understands about getting values
 * */
trait FindOrmEntityAndField[Schema[_]] {
  def apply[T](s: Schema[T]): List[OrmValueGetter[_]]
}

/** the orm getter is 'given this data from the database table what is the result. Path and index and where to put the result when we get it */
case class OrmGettersAndPath(ormValueGetters: Array[OrmValueGetter[_]], path: Array[Array[Int]], indicies: Array[Int]) {
  def fieldTypes: Array[FieldType[_]] = ormValueGetters.flatMap(_.fieldTypes)
//  def toForThisRow(list: List[String]): OrmGettersForThisRowAndPath = OrmGettersForThisRowAndPath(ormValueGetters.map(_.ormValueGetterForARow(list)), path, indicies)
}

////Could flatten a little: this is all about one table...
//case class OrmGettersForThisRowAndPath(ormValueGetters: Array[OrmValueGetterForARow[_]], path: Array[Array[Int]], indicies: Array[Int]) {
//  def fieldTypeWithIndexs: Array[FieldTypeAndIndex[_]] = ormValueGetters.flatMap(_.fieldTypes)
//  def prettyString = {ormValueGetters.zip(path).zip(indicies).map { case ((g, p), i) => s"${g.fieldTypes.map(fti => s"(${fti.fieldType.name}:${fti.index}").mkString(",")},${p.mkString(",")},$i)" } }.mkString(",")
//}

case class TablesAndFieldsAndPaths(map: Map[TableName, OrmGettersAndPath]) {
  def getOrmGettersAndPath(tableName: TableName): OrmGettersAndPath = map.getOrElse(tableName, throw new RuntimeException(s"Cannot find the table ${tableName.tableName} in the known tables: [${map.keys.map(_.tableName).mkString(",")}]"))
  def prettyPrint: List[String] = map.toList.sortBy(_._1.tableName).flatMap { case (table, OrmGettersAndPath(ormValueGetters, paths, indicies)) =>
    table.tableName :: ormValueGetters.toList.zip(paths).zip(indicies).zipWithIndex.map {
      case (((og, path), index), i) => s"   $i ${og.fieldTypes.map(_.prettyPrint).mkString(",")} - (${path.mkString(",")}) - $index"
    }
  }
  def ormFactory[Schema[_]](keys: OrmKeys[Schema])(implicit findOrmEntityAndField: FindOrmEntityAndField[Schema]): OrmFactory[Schema] =
    new OrmFactoryImpl[Schema](keys, this)
}
object EntityAndPath {
  def apply[Schema[_]](keys: OrmKeys[Schema])(implicit findOrmEntityAndField: FindOrmEntityAndField[Schema]): TablesAndFieldsAndPaths =
    TablesAndFieldsAndPaths(keys.allKeys.flatMap { key => findOrmEntityAndField(key.t).map { ormValueGetter => (ormValueGetter.tableName, ormValueGetter, key.path, key.index) }
    }.groupBy(_._1).map {
      case (tableName, list) => (tableName, OrmGettersAndPath(list.map(_._2).toArray, list.map(_._3.toArray).toArray, list.map(_._4).toArray))
    })
}

case class EntityAndFieldsAndPath[E <: OrmEntity](entity: E, fieldsAndPath: OrmGettersAndPath) {
  entity.validate
}
trait OrmFactory[Schema[_]] {
  def mainEntity(tableName: TableName, alias: String, primaryKey: Keys, children: List[EntityAndFieldsAndPath[_ <: ChildEntity]]): EntityAndFieldsAndPath[MainEntity]
  def manyToOneEntity(tableName: TableName, alias: String, primaryKey: Keys, idInParent: Keys, children: List[EntityAndFieldsAndPath[_ <: ChildEntity]]): EntityAndFieldsAndPath[ManyToOneEntity]
  def sameIdEntity(tableName: TableName, alias: String, primaryKey: Keys, children: List[EntityAndFieldsAndPath[_ <: ChildEntity]]): EntityAndFieldsAndPath[SameIdEntity]
  def oneToManyEntity(tableName: TableName, alias: String, primaryKey: Keys, parentId: Keys, children: List[EntityAndFieldsAndPath[_ <: ChildEntity]]): EntityAndFieldsAndPath[OneToManyEntity]
}
class OrmFactoryImpl[Schema[_]](keys: OrmKeys[Schema], tablesAndFieldsAndPaths: TablesAndFieldsAndPaths)(implicit findOrmEntityAndField: FindOrmEntityAndField[Schema]) extends OrmFactory[Schema] {
  override def mainEntity(tableName: TableName, alias: String, primaryKey: Keys, children: List[EntityAndFieldsAndPath[_ <: ChildEntity]]): EntityAndFieldsAndPath[MainEntity] = {
    val fieldsAndPath: OrmGettersAndPath = tablesAndFieldsAndPaths.getOrmGettersAndPath(tableName)
    val entity = MainEntity(tableName, alias, primaryKey, fieldsAndPath.fieldTypes.toList, children.map(_.entity))
    EntityAndFieldsAndPath(entity, fieldsAndPath)
  }
  override def manyToOneEntity(tableName: TableName, alias: String, primaryKey: Keys, idInParent: Keys, children: List[EntityAndFieldsAndPath[_ <: ChildEntity]]): EntityAndFieldsAndPath[ManyToOneEntity] = {
    val fieldsAndPath: OrmGettersAndPath = tablesAndFieldsAndPaths.getOrmGettersAndPath(tableName)
    val entity = ManyToOneEntity(tableName, alias, primaryKey, idInParent, fieldsAndPath.fieldTypes.toList, children.map(_.entity))
    EntityAndFieldsAndPath(entity, fieldsAndPath)
  }
  override def sameIdEntity(tableName: TableName, alias: String, primaryKey: Keys, children: List[EntityAndFieldsAndPath[_ <: ChildEntity]]): EntityAndFieldsAndPath[SameIdEntity] = {
    val fieldsAndPath: OrmGettersAndPath = tablesAndFieldsAndPaths.getOrmGettersAndPath(tableName)
    val entity = SameIdEntity(tableName, alias, primaryKey, fieldsAndPath.fieldTypes.toList, children.map(_.entity))
    EntityAndFieldsAndPath(entity, fieldsAndPath)
  }
  override def oneToManyEntity(tableName: TableName, alias: String, primaryKey: Keys, parentId: Keys, children: List[EntityAndFieldsAndPath[_ <: ChildEntity]]): EntityAndFieldsAndPath[OneToManyEntity] = {
    val fieldsAndPath: OrmGettersAndPath = tablesAndFieldsAndPaths.getOrmGettersAndPath(tableName)
    val entity = OneToManyEntity(tableName, alias, primaryKey, parentId, fieldsAndPath.fieldTypes.toList, children.map(_.entity))
    EntityAndFieldsAndPath(entity, fieldsAndPath)
  }
}



