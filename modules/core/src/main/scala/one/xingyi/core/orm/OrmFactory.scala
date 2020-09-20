package one.xingyi.core.orm

case class EntityAndPath[E <: OrmEntity](entity: E, paths: Array[List[Int]]) {
  require(entity.fieldsForCreate.size == paths.size, s"Should have same number of paths as have fieldsForCreate. In entity ${entity.fieldsForCreate.size} in paths: ${paths.size}")
}

trait FindOrmEntityAndField[T] extends (T => Option[(TableName, FieldType[_])])

case class FieldsAndPath(fieldType: List[FieldType[_]], path: Array[Array[Int]], indicies: Array[Int]) {}

case class TablesAndFieldsAndPaths(map: Map[TableName, FieldsAndPath]) {
  def prettyPrint: List[String] = map.toList.sortBy(_._1.tableName).flatMap { case (table, FieldsAndPath(fieldTypes, paths, indicies)) =>
    table.tableName :: fieldTypes.zip(paths).zip(indicies).zipWithIndex.map { case (((ft, path), index), i) => s"   $i ${ft.name} - (${path.mkString(",")}) - $index" }
  }
  def ormFactory(keys: NumericKeys[_]): OrmFactory = new OrmFactoryImpl(keys, this)
}
object EntityAndPath {
  def apply[T](keys: NumericKeys[T])(implicit findOrmEntityAndField: FindOrmEntityAndField[T]): TablesAndFieldsAndPaths = {
    TablesAndFieldsAndPaths(keys.allKeys.flatMap { key => findOrmEntityAndField(key.t).map(tf => (tf._1, tf._2, key.path, key.index)) }.groupBy(_._1).map {
      case (tableName, list) => (tableName, FieldsAndPath(list.map(_._2), list.map(_._3.toArray).toArray, list.map(_._4).toArray))
    })
  }
}

case class EntityAndFieldsAndPath[E <: OrmEntity](entity: E, fieldsAndPath: FieldsAndPath)
trait OrmFactory {
  def ormMaker: OrmMaker[Array[Any]]

  def mainEntity(tableName: TableName, alias: String, primaryKey: Keys, children: List[EntityAndFieldsAndPath[_ <: ChildEntity]]): EntityAndFieldsAndPath[MainEntity]
  def manyToOneEntity(tableName: TableName, alias: String, primaryKey: Keys, idInParent: Keys, children: List[EntityAndFieldsAndPath[_ <: ChildEntity]]): EntityAndFieldsAndPath[ManyToOneEntity]
  def sameIdEntity(tableName: TableName, alias: String, primaryKey: Keys, children: List[EntityAndFieldsAndPath[_ <: ChildEntity]]): EntityAndFieldsAndPath[SameIdEntity]
  def oneToManyEntity(tableName: TableName, alias: String, primaryKey: Keys, parentId: Keys, children: List[EntityAndFieldsAndPath[_ <: ChildEntity]]): EntityAndFieldsAndPath[OneToManyEntity]
}
class OrmFactoryImpl(keys: NumericKeys[_], tablesAndFieldsAndPaths: TablesAndFieldsAndPaths) extends OrmFactory {
  override def mainEntity(tableName: TableName, alias: String, primaryKey: Keys, children: List[EntityAndFieldsAndPath[_ <: ChildEntity]]): EntityAndFieldsAndPath[MainEntity] = {
    val fieldsAndPath: FieldsAndPath = tablesAndFieldsAndPaths.map(tableName)
    val entity = MainEntity(tableName, alias, primaryKey, fieldsAndPath.fieldType, children.map(_.entity))
    EntityAndFieldsAndPath(entity, fieldsAndPath)
  }
  override def manyToOneEntity(tableName: TableName, alias: String, primaryKey: Keys, idInParent: Keys, children: List[EntityAndFieldsAndPath[_ <: ChildEntity]]): EntityAndFieldsAndPath[ManyToOneEntity] = {
    val fieldsAndPath: FieldsAndPath = tablesAndFieldsAndPaths.map(tableName)
    val entity = ManyToOneEntity(tableName, alias, primaryKey, idInParent, fieldsAndPath.fieldType, children.map(_.entity))
    EntityAndFieldsAndPath(entity, fieldsAndPath)
  }
  override def sameIdEntity(tableName: TableName, alias: String, primaryKey: Keys, children: List[EntityAndFieldsAndPath[_ <: ChildEntity]]): EntityAndFieldsAndPath[SameIdEntity] = {
    val fieldsAndPath: FieldsAndPath = tablesAndFieldsAndPaths.map(tableName)
    val entity = SameIdEntity(tableName, alias, primaryKey, fieldsAndPath.fieldType, children.map(_.entity))
    EntityAndFieldsAndPath(entity, fieldsAndPath)
  }
  override def oneToManyEntity(tableName: TableName, alias: String, primaryKey: Keys, parentId: Keys, children: List[EntityAndFieldsAndPath[_ <: ChildEntity]]): EntityAndFieldsAndPath[OneToManyEntity] = {
    val fieldsAndPath: FieldsAndPath = tablesAndFieldsAndPaths.map(tableName)
    val entity = OneToManyEntity(tableName, alias, primaryKey, parentId, fieldsAndPath.fieldType, children.map(_.entity))
    EntityAndFieldsAndPath(entity, fieldsAndPath)
  }
  override def ormMaker: OrmMaker[Array[Any]] = new OrmMakerForArrayAny(keys, tablesAndFieldsAndPaths)
}

class OrmMakerForArrayAny[T](numericKeys: NumericKeys[T], tablesAndFieldsAndPaths: TablesAndFieldsAndPaths) extends OrmMaker[Array[Any]] {
  override def apply(mainEntity: MainEntity): Map[OrmEntity, List[List[AnyRef]]] => Stream[Array[Any]] = { data =>
    val mainData = data(mainEntity)
    val fieldsAndPath = tablesAndFieldsAndPaths.map(mainEntity.tableName)
    val paths = fieldsAndPath.path
    val indicies = fieldsAndPath.indicies
    mainData.map { oneRow =>

    }
    ???
  }
}
