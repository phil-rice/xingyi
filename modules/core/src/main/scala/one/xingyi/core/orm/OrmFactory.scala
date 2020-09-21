package one.xingyi.core.orm

case class EntityAndPath[E <: OrmEntity](entity: E, paths: Array[List[Int]]) {
  require(entity.fieldsForCreate.size == paths.size, s"Should have same number of paths as have fieldsForCreate. In entity ${entity.fieldsForCreate.size} in paths: ${paths.size}")
}

case class FieldTypeAndIndex[T](fieldType: FieldType[T], index: Int)

trait OrmValueTransformer[T] extends ((Array[FieldTypeAndIndex[_]], Array[Any]) => Any)
object OrmValueTransformer {
  implicit def defaultOrmValueTransformer[T]: OrmValueTransformer[T] = new OrmValueTransformer[T] {
    override def apply(v1: Array[FieldTypeAndIndex[_]], v2: Array[Any]): Any = {
      require(v1.size == 1, s"Cannot transform using defaultValueTransformer if more than one value in ${v1}")
      v2(v1.head.index)
    }
  }
}

case class OrmValueGetter[T](tableName: TableName, fieldTypes: List[FieldType[_]])(implicit val tx: OrmValueTransformer[T])


/** For example a single item in the schema might be in several places in the database (a key/a foreign key). That item might be represented by multiple fields (e.g. a composite string, or a date where the date is stored in multiple fields_ */
trait FindOrmEntityAndField[Schema] extends (Schema => List[OrmValueGetter[_]])

case class FieldsAndPath(ormValueGetters: Array[OrmValueGetter[_]], path: Array[Array[Int]], indicies: Array[Int]) {
  def fieldTypes: Array[FieldType[_]] = ormValueGetters.flatMap(_.fieldTypes)
}

case class TablesAndFieldsAndPaths(map: Map[TableName, FieldsAndPath]) {
  def getFieldsAndPath(tableName: TableName) = map.getOrElse(tableName, throw new RuntimeException(s"Cannot find the table ${tableName.tableName} in the known tables: [${map.keys.map(_.tableName).mkString(",")}]"))
  def prettyPrint: List[String] = map.toList.sortBy(_._1.tableName).flatMap { case (table, FieldsAndPath(ormValueGetters, paths, indicies)) =>
    table.tableName :: ormValueGetters.toList.zip(paths).zip(indicies).zipWithIndex.map { case (((og, path), index), i) => s"   $i ${og.fieldTypes.map(_.name).mkString(",")} - (${path.mkString(",")}) - $index" }
  }
  def ormFactory(keys: NumericKeys[_]): OrmFactory = new OrmFactoryImpl(keys, this)
}
object EntityAndPath {
  def apply[T](keys: NumericKeys[T])(implicit findOrmEntityAndField: FindOrmEntityAndField[T]): TablesAndFieldsAndPaths =
    TablesAndFieldsAndPaths(keys.allKeys.flatMap { key => findOrmEntityAndField(key.t).map { ormValueGetter => (ormValueGetter.tableName, ormValueGetter, key.path, key.index) }
    }.groupBy(_._1).map {
      case (tableName, list) => (tableName, FieldsAndPath(list.map(_._2).toArray, list.map(_._3.toArray).toArray, list.map(_._4).toArray))
    })
}

case class EntityAndFieldsAndPath[E <: OrmEntity](entity: E, fieldsAndPath: FieldsAndPath)
trait OrmFactory {
  def ormMaker(map: Map[OneToManyEntity, NumericKey[_]]): OrmMaker[Array[Any]]

  def mainEntity(tableName: TableName, alias: String, primaryKey: Keys, children: List[EntityAndFieldsAndPath[_ <: ChildEntity]]): EntityAndFieldsAndPath[MainEntity]
  def manyToOneEntity(tableName: TableName, alias: String, primaryKey: Keys, idInParent: Keys, children: List[EntityAndFieldsAndPath[_ <: ChildEntity]]): EntityAndFieldsAndPath[ManyToOneEntity]
  def sameIdEntity(tableName: TableName, alias: String, primaryKey: Keys, children: List[EntityAndFieldsAndPath[_ <: ChildEntity]]): EntityAndFieldsAndPath[SameIdEntity]
  def oneToManyEntity(tableName: TableName, alias: String, primaryKey: Keys, parentId: Keys, children: List[EntityAndFieldsAndPath[_ <: ChildEntity]]): EntityAndFieldsAndPath[OneToManyEntity]
}
class OrmFactoryImpl(keys: NumericKeys[_], tablesAndFieldsAndPaths: TablesAndFieldsAndPaths) extends OrmFactory {
  override def mainEntity(tableName: TableName, alias: String, primaryKey: Keys, children: List[EntityAndFieldsAndPath[_ <: ChildEntity]]): EntityAndFieldsAndPath[MainEntity] = {
    val fieldsAndPath: FieldsAndPath = tablesAndFieldsAndPaths.getFieldsAndPath(tableName)
    val entity = MainEntity(tableName, alias, primaryKey, fieldsAndPath.fieldTypes.toList, children.map(_.entity))
    EntityAndFieldsAndPath(entity, fieldsAndPath)
  }
  override def manyToOneEntity(tableName: TableName, alias: String, primaryKey: Keys, idInParent: Keys, children: List[EntityAndFieldsAndPath[_ <: ChildEntity]]): EntityAndFieldsAndPath[ManyToOneEntity] = {
    val fieldsAndPath: FieldsAndPath = tablesAndFieldsAndPaths.getFieldsAndPath(tableName)
    val entity = ManyToOneEntity(tableName, alias, primaryKey, idInParent, fieldsAndPath.fieldTypes.toList, children.map(_.entity))
    EntityAndFieldsAndPath(entity, fieldsAndPath)
  }
  override def sameIdEntity(tableName: TableName, alias: String, primaryKey: Keys, children: List[EntityAndFieldsAndPath[_ <: ChildEntity]]): EntityAndFieldsAndPath[SameIdEntity] = {
    val fieldsAndPath: FieldsAndPath = tablesAndFieldsAndPaths.getFieldsAndPath(tableName)
    val entity = SameIdEntity(tableName, alias, primaryKey, fieldsAndPath.fieldTypes.toList, children.map(_.entity))
    EntityAndFieldsAndPath(entity, fieldsAndPath)
  }
  override def oneToManyEntity(tableName: TableName, alias: String, primaryKey: Keys, parentId: Keys, children: List[EntityAndFieldsAndPath[_ <: ChildEntity]]): EntityAndFieldsAndPath[OneToManyEntity] = {
    val fieldsAndPath: FieldsAndPath = tablesAndFieldsAndPaths.getFieldsAndPath(tableName)
    val entity = OneToManyEntity(tableName, alias, primaryKey, parentId, fieldsAndPath.fieldTypes.toList, children.map(_.entity))
    EntityAndFieldsAndPath(entity, fieldsAndPath)
  }
  override def ormMaker(map: Map[OneToManyEntity, NumericKey[_]]): OrmMaker[Array[Any]] =
    new OrmMakerForArrayAny(keys, tablesAndFieldsAndPaths, map.map { case (k, v) => (k, (v.path :+ v.index).toArray) })
}

class OrmMakerForArrayAny[T](numericKeys: NumericKeys[T], tablesAndFieldsAndPaths: TablesAndFieldsAndPaths, oneToManyPathMap: Map[OneToManyEntity, Array[Int]]) extends OrmMaker[Array[Any]] {
  println("OrmMakerForArrayAny" + oneToManyPathMap.map { case (e, path) => s"${e.tableName} -> ${path.toList}" })

  def processAllDataForManyToOne(parent: OrmEntity, parentData: List[(Any, List[AnyRef], Array[Any])], data: Map[OrmEntity, List[List[AnyRef]]])(entity: ManyToOneEntity) = {
    val manyToOneData: Map[Any, List[AnyRef]] = data(entity).map(oneRow => entity.primaryKeyFieldsAndIndex.getKey(oneRow) -> oneRow).toMap
    val parentIdsAndIndex = entity.idInParent.toKeysAndIndex(parent)
    parentData.foreach { case (_, mainRow, ar) =>
      val manyToOneRow = manyToOneData(parentIdsAndIndex.getKey(mainRow))
      populateArray(entity, manyToOneRow, ar)
    }
  }
  def processAllDataForOneToMany(parent: OrmEntity, arrays: Map[Any, Array[Any]], data: Map[OrmEntity, List[List[AnyRef]]])(entity: OneToManyEntity) = {
    println("processAllDataForOneToMany")
    println("   arrays:\n" + arrays.map { case (id, data) => s"${id} -> ${data.mkString(",")}" }.mkString("\n"))
    val nextPath = oneToManyPathMap(entity)
    data(entity).foreach { oneRow =>
      val parentsKey = entity.parentIdsAndIndex.getKey(oneRow)
      val ar = arrays(parentsKey)
      numericKeys.next(nextPath, ar)
      populateArray(entity, oneRow, ar)
    }
  }

  def makeIdArrayForManyToOne(parentEntity: OrmEntity, data: Map[OrmEntity, List[List[AnyRef]]])(entity: ManyToOneEntity) =
    (entity, entity.idInParent.toKeysAndIndex(parentEntity), data(entity).map(oneRow => entity.primaryKeyFieldsAndIndex.getKey(oneRow) -> oneRow).toMap)

  def processAllDataForSameIds(parent: OrmEntity, arrays: Map[Any, Array[Any]], data: Map[OrmEntity, List[List[AnyRef]]])(entity: SameIdEntity) =
    data(entity).foreach { oneRow => populateArray(entity, oneRow, arrays(entity.primaryKeyFieldsAndIndex.getKey(oneRow))) }

  override def apply(mainEntity: MainEntity): Map[OrmEntity, List[List[AnyRef]]] => Stream[Array[Any]] = { data =>
    val mainData = data(mainEntity)
    val arrays: List[(Any, List[AnyRef], Array[Any])] = mainData.map { oneRow =>
      val ar = numericKeys.makeAndSetupArray
      populateArray(mainEntity, oneRow, ar)
      (mainEntity.primaryKeyFieldsAndIndex.getKey(oneRow), oneRow, ar)
    }
    val arraysAsMap = arrays.map(t => (t._1, t._3)).toMap
    mainEntity.children.collect { case e: ManyToOneEntity => e }.foreach(processAllDataForManyToOne(mainEntity, arrays, data))
    mainEntity.children.collect { case e: OneToManyEntity => e }.foreach(processAllDataForOneToMany(mainEntity, arraysAsMap, data))
    mainEntity.children.collect { case e: SameIdEntity => e }.foreach(processAllDataForSameIds(mainEntity, arraysAsMap, data))

    arrays.map(_._3).toStream
  }
  def populateArray(entity: OrmEntity, oneRow: List[AnyRef], ar: Array[Any]) = {
    val fieldsAndPath = tablesAndFieldsAndPaths.map(entity.tableName)
    val paths = fieldsAndPath.path
    val indicies = fieldsAndPath.indicies
    var i = 0
    println(s"Entity: ${entity.tableName}")
    while (i < paths.size) {
      val d = oneRow(i)
      println(s"   d $d  i $i  paths ${paths.map(_.toList).mkString(";")} ${indicies.mkString(",")}")
      println(s"   path: [${paths(i).mkString(",")}]  indicies: ${indicies(i)}")
      numericKeys.put(paths(i), indicies(i), ar, d)
      i += 1
    }
  }
}
