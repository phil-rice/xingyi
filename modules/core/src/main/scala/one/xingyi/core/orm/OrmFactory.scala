package one.xingyi.core.orm

import scala.language.higherKinds

object OrmFactory {
  def apply[Context, Schema[_]](schema: Schema[_])(implicit toTableAndFieldTypes: ToTableAndFieldTypes[Context, Schema], schemaMapKey: SchemaMapKey[Schema]): OrmFactory[Schema] =
    new OrmFactoryImpl(schema)
}

trait OrmFactory[Schema[_]] {
  def mainEntity(tableName: TableName, alias: String, primaryKey: Keys, children: List[ChildEntity]): MainEntity
  def manyToOneEntity(tableName: TableName, alias: String, primaryKey: Keys, idInParent: Keys, children: List[ChildEntity]): ManyToOneEntity
  def sameIdEntity(tableName: TableName, alias: String, primaryKey: Keys, children: List[ChildEntity]): SameIdEntity
  def oneToManyEntity(tableName: TableName, alias: String, primaryKey: Keys, parentId: Keys, children: List[ChildEntity]): OneToManyEntity
}

class OrmFactoryImpl[Context, Schema[_]](schema: Schema[_])(implicit toTableAndFieldTypes: ToTableAndFieldTypes[Context, Schema], schemaMapKey: SchemaMapKey[Schema]) extends OrmFactory[Schema] {
  private val tableToFieldTypes: Map[TableName, List[FieldType[_]]] =
    schemaMapKey.descendants(schema).
      flatMap { s: Schema[_] => toTableAndFieldTypes.apply(s).map(tf => tf.tableName -> tf.fieldTypes) }.
      groupBy(_._1).map { case (tn, list) => tn -> list.flatMap(_._2) }

  def mainEntity(tableName: TableName, alias: String, primaryKey: Keys, children: List[ChildEntity]): MainEntity =
    MainEntity(tableName, alias, primaryKey, tableToFieldTypes(tableName), children)

  def manyToOneEntity(tableName: TableName, alias: String, primaryKey: Keys, idInParent: Keys, children: List[ChildEntity]): ManyToOneEntity =
    ManyToOneEntity(tableName, alias, primaryKey, idInParent, tableToFieldTypes(tableName), children)

  def sameIdEntity(tableName: TableName, alias: String, primaryKey: Keys, children: List[ChildEntity]): SameIdEntity =
    SameIdEntity(tableName, alias, primaryKey, tableToFieldTypes(tableName), children)

  def oneToManyEntity(tableName: TableName, alias: String, primaryKey: Keys, parentId: Keys, children: List[ChildEntity]): OneToManyEntity =
    OneToManyEntity(tableName, alias, primaryKey, parentId, tableToFieldTypes(tableName), children)
}





