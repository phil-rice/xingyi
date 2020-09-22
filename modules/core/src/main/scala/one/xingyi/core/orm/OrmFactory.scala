package one.xingyi.core.orm

import java.util.concurrent.atomic.AtomicInteger

case class EntityAndPath[E <: OrmEntity](entity: E, paths: Array[List[Int]]) {
  require(entity.fieldsForCreate.size == paths.size, s"Should have same number of paths as have fieldsForCreate. In entity ${entity.fieldsForCreate.size} in paths: ${paths.size}")
}

case class FieldTypeAndIndex[T](fieldType: FieldType[T], index: Int)

trait OrmValueTransformer[T] extends ((Array[FieldTypeAndIndex[_]], Array[Any]) => Any)
object OrmValueTransformer {
  implicit def defaultOrmValueTransformer[T]: OrmValueTransformer[T] = new OrmValueTransformer[T] {
    override def apply(v1: Array[FieldTypeAndIndex[_]], v2: Array[Any]): Any = {
      require(v1.size == 1, s"Cannot transform using defaultValueTransformer if more than one value in ${v1}")
      val result = v2(v1.head.index)
      println(s"tx(${v1.head.fieldType}) = $result")
      result
    }
  }
}

case class OrmValueGetter[T](tableName: TableName, fieldTypes: List[FieldType[_]])(implicit val tx: OrmValueTransformer[T]) {
  def ormValueGetterForARow(list: List[String]) = OrmValueGetterForARow(tableName, fieldTypes.map(_.withIndex(list)).toArray, tx)
}
case class OrmValueGetterForARow[T](tableName: TableName, fieldTypes: Array[FieldTypeAndIndex[_]], tx: OrmValueTransformer[T]) {
  def apply(oneRow: Array[Any]) = tx(fieldTypes, oneRow)
}


/** For example a single item in the schema might be in several places in the database (a key/a foreign key). That item might be represented by multiple fields (e.g. a composite string, or a date where the date is stored in multiple fields_ */
trait FindOrmEntityAndField[Schema] extends (Schema => List[OrmValueGetter[_]])

/** the orm getter is 'given this data from the database table what is the result. Path and index and where to put the result when we get it */
case class OrmGettersAndPath(ormValueGetters: Array[OrmValueGetter[_]], path: Array[Array[Int]], indicies: Array[Int]) {
  def fieldTypes: Array[FieldType[_]] = ormValueGetters.flatMap(_.fieldTypes)
  def toForThisRow(list: List[String]): OrmGettersForThisRowAndPath = OrmGettersForThisRowAndPath(ormValueGetters.map(_.ormValueGetterForARow(list)), path, indicies)
}

//Could flatten a little: this is all about one table...
case class OrmGettersForThisRowAndPath(ormValueGetters: Array[OrmValueGetterForARow[_]], path: Array[Array[Int]], indicies: Array[Int]) {
  def fieldTypeWithIndexs: Array[FieldTypeAndIndex[_]] = ormValueGetters.flatMap(_.fieldTypes)
  def prettyString = {ormValueGetters.zip(path).zip(indicies).map { case ((g, p), i) => s"${g.fieldTypes.map(fti => s"(${fti.fieldType.name}:${fti.index}").mkString(",")},${p.mkString(",")},$i)" } }.mkString(",")
}

case class TablesAndFieldsAndPaths(map: Map[TableName, OrmGettersAndPath]) {
  def getOrmGettersAndPath(tableName: TableName): OrmGettersAndPath = map.getOrElse(tableName, throw new RuntimeException(s"Cannot find the table ${tableName.tableName} in the known tables: [${map.keys.map(_.tableName).mkString(",")}]"))
  def prettyPrint: List[String] = map.toList.sortBy(_._1.tableName).flatMap { case (table, OrmGettersAndPath(ormValueGetters, paths, indicies)) =>
    table.tableName :: ormValueGetters.toList.zip(paths).zip(indicies).zipWithIndex.map { case (((og, path), index), i) => s"   $i ${og.fieldTypes.map(_.name).mkString(",")} - (${path.mkString(",")}) - $index" }
  }
  def ormFactory(keys: NumericKeys[_]): OrmFactory = new OrmFactoryImpl(keys, this)
}
object EntityAndPath {
  def apply[T](keys: NumericKeys[T])(implicit findOrmEntityAndField: FindOrmEntityAndField[T]): TablesAndFieldsAndPaths =
    TablesAndFieldsAndPaths(keys.allKeys.flatMap { key => findOrmEntityAndField(key.t).map { ormValueGetter => (ormValueGetter.tableName, ormValueGetter, key.path, key.index) }
    }.groupBy(_._1).map {
      case (tableName, list) => (tableName, OrmGettersAndPath(list.map(_._2).toArray, list.map(_._3.toArray).toArray, list.map(_._4).toArray))
    })
}

case class EntityAndFieldsAndPath[E <: OrmEntity](entity: E, fieldsAndPath: OrmGettersAndPath)
trait OrmFactory {
  def ormMaker(map: Map[OneToManyEntity, NumericKey[_]]): OrmMaker[Array[Any]]

  def mainEntity(tableName: TableName, alias: String, primaryKey: Keys, children: List[EntityAndFieldsAndPath[_ <: ChildEntity]]): EntityAndFieldsAndPath[MainEntity]
  def manyToOneEntity(tableName: TableName, alias: String, primaryKey: Keys, idInParent: Keys, children: List[EntityAndFieldsAndPath[_ <: ChildEntity]]): EntityAndFieldsAndPath[ManyToOneEntity]
  def sameIdEntity(tableName: TableName, alias: String, primaryKey: Keys, children: List[EntityAndFieldsAndPath[_ <: ChildEntity]]): EntityAndFieldsAndPath[SameIdEntity]
  def oneToManyEntity(tableName: TableName, alias: String, primaryKey: Keys, parentId: Keys, children: List[EntityAndFieldsAndPath[_ <: ChildEntity]]): EntityAndFieldsAndPath[OneToManyEntity]
}
class OrmFactoryImpl(keys: NumericKeys[_], tablesAndFieldsAndPaths: TablesAndFieldsAndPaths) extends OrmFactory {
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
  override def ormMaker(map: Map[OneToManyEntity, NumericKey[_]]): OrmMaker[Array[Any]] =
    new OrmMakerForArrayAny(keys, tablesAndFieldsAndPaths, map.map { case (k, v) => (k, (v.path :+ v.index).toArray) })
}

class StreamArrayAnyForOneEntity[T](numericKeys: NumericKeys[T], mainEntity: MainEntity, tablesAndFieldsAndPaths: TablesAndFieldsAndPaths, oneToManyPathMap: Map[OneToManyEntity, Array[Int]])
  extends (Map[OrmEntity, List[List[AnyRef]]] => Stream[Array[Any]]) {
  val aliasToOrmGetters: Map[String, OrmGettersForThisRowAndPath] = (mainEntity :: mainEntity.children).map { entity =>
    (entity.alias, tablesAndFieldsAndPaths.getOrmGettersAndPath(entity.tableName).toForThisRow(entity.fieldsForCreate.map(_.name)))
  }.toMap

  //  println(s"StreamArrayForOneEntity(${mainEntity.tableName}\n${aliasToOrmGetters.map { case (a, getter) => s"$a,${getter.prettyString}" }.mkString("\n")}")

  def apply(data: Map[OrmEntity, List[List[AnyRef]]]): Stream[Array[Any]] = {
    val mainData = data(mainEntity)
    val arrays: List[(Any, List[AnyRef], Array[Any])] = mainData.map { oneRow =>
      val ar = numericKeys.makeAndSetupArray
      populateArray(mainEntity, oneRow, ar)
      (mainEntity.primaryKeyFieldsAndIndex.getKey(oneRow), oneRow, ar)
    }
    val arraysAsMap: Map[Any, Array[Any]] = arrays.map(t => (t._1, t._3)).toMap


    mainEntity.children.collect { case e: ManyToOneEntity => e }.foreach(processAllDataForManyToOne(mainEntity, arrays, data))
    mainEntity.children.collect { case e: OneToManyEntity => e }.foreach(processAllDataForOneToMany(mainEntity, arraysAsMap, data))
    mainEntity.children.collect { case e: SameIdEntity => e }.foreach(processAllDataForSameIds(mainEntity, arraysAsMap, data))

    arrays.map(_._3).toStream
  }

  def processAllDataForManyToOne(parent: OrmEntity, parentData: List[(Any, List[AnyRef], Array[Any])], data: Map[OrmEntity, List[List[AnyRef]]])(entity: ManyToOneEntity) {
    val manyToOneData: Map[Any, List[AnyRef]] = data(entity).map(oneRow => entity.primaryKeyFieldsAndIndex.getKey(oneRow) -> oneRow).toMap
    val parentIdsAndIndex = entity.idInParent.toKeysAndIndex(parent)
    parentData.foreach { case (_, mainRow, ar) =>
      val manyToOneRow = manyToOneData(parentIdsAndIndex.getKey(mainRow))
      populateArray(entity, manyToOneRow, ar)
    }
  }

  private def processAllDataForOneToMany(parent: OrmEntity, arrays: Map[Any, Array[Any]], data: Map[OrmEntity, List[List[AnyRef]]])(entity: OneToManyEntity) {
    val nextPath = oneToManyPathMap(entity)
    data(entity).foreach { oneRow =>
      val parentsKey = entity.parentIdsAndIndex.getKey(oneRow)
      val ar = arrays(parentsKey)
      numericKeys.next(nextPath, ar)
      populateArray(entity, oneRow, ar)
    }
  }

  def populateArray(entity: OrmEntity, oneRow: List[AnyRef], ar: Array[Any]) {
    val fieldsAndPath = aliasToOrmGetters(entity.alias)
    val ormGetters: Array[OrmValueGetterForARow[_]] = fieldsAndPath.ormValueGetters
    val paths = fieldsAndPath.path
    val indicies = fieldsAndPath.indicies
    var i = 0
    while (i < ormGetters.length) {
      val d = ormGetters(i) apply (oneRow.toArray)
      numericKeys.put(paths(i), indicies(i), ar, d)
      i += 1
    }
  }

  //  private def makeIdArrayForManyToOne(parentEntity: OrmEntity, data: Map[OrmEntity, List[List[AnyRef]]])(entity: ManyToOneEntity) =
  //    (entity, entity.idInParent.toKeysAndIndex(parentEntity), data(entity).map(oneRow => entity.primaryKeyFieldsAndIndex.getKey(oneRow) -> oneRow).toMap)

  private def processAllDataForSameIds(parent: OrmEntity, arrays: Map[Any, Array[Any]], data: Map[OrmEntity, List[List[AnyRef]]])(entity: SameIdEntity) =
    data(entity).foreach { oneRow => populateArray(entity, oneRow, arrays(entity.primaryKeyFieldsAndIndex.getKey(oneRow))) }

}

class OrmMakerForArrayAny[Schema](numericKeys: NumericKeys[Schema], tablesAndFieldsAndPaths: TablesAndFieldsAndPaths, oneToManyPathMap: Map[OneToManyEntity, Array[Int]]) extends OrmMaker[Array[Any]] {
  private val created = new AtomicInteger(0)
  def createdCount: Int = created.get
  override def apply(mainEntity: MainEntity): Map[OrmEntity, List[List[AnyRef]]] => Stream[Array[Any]] = {
    created.incrementAndGet()
    new StreamArrayAnyForOneEntity[Schema](numericKeys, mainEntity, tablesAndFieldsAndPaths, oneToManyPathMap)
  }
}
