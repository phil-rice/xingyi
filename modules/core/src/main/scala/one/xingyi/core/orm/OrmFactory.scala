package one.xingyi.core.orm

import scala.language.higherKinds

object OrmFactory {
  def apply[Context, Schema[_]](schema: Schema[_])(implicit toTableAndFieldTypes: ToAliasAndFieldTypes[Context, Schema], schemaMapKey: SchemaMapKey[Schema]): OrmFactory[Schema] =
    new OrmFactoryImpl(schema)
}

trait OrmFactory[Schema[_]] {
  def mainEntity(alias: Alias, aliasToBeRemoved: String, primaryKey: Keys, children: List[ChildEntity]): MainEntity
  def manyToOneEntity(alias: Alias, alias1: String, primaryKey: Keys, idInParent: Keys, children: List[ChildEntity]): ManyToOneEntity
  def sameIdEntity(alias: Alias, alias1: String, primaryKey: Keys, children: List[ChildEntity]): SameIdEntity
  def oneToManyEntity(alias: Alias, alias1: String, primaryKey: Keys, parentId: Keys, children: List[ChildEntity]): OneToManyEntity
}

class OrmFactoryImpl[Context, Schema[_]](schema: Schema[_])(implicit toTableAndFieldTypes: ToAliasAndFieldTypes[Context, Schema], schemaMapKey: SchemaMapKey[Schema]) extends OrmFactory[Schema] {
  private val tableToFieldTypes: Map[Alias, List[FieldType[_]]] =
    schemaMapKey.descendants(schema).
      flatMap { s: Schema[_] => toTableAndFieldTypes.apply(s).map(tf => tf.alias -> tf.fieldTypes) }.
      groupBy(_._1).map { case (alias, list) => alias -> list.flatMap(_._2) }

  def mainEntity(alias: Alias, aliasToBeRemoved: String, primaryKey: Keys, children: List[ChildEntity]): MainEntity =
    MainEntity(alias, aliasToBeRemoved, primaryKey, tableToFieldTypes(alias), children)

  def manyToOneEntity(alias: Alias, alias1: String, primaryKey: Keys, idInParent: Keys, children: List[ChildEntity]): ManyToOneEntity =
    ManyToOneEntity(alias, alias1, primaryKey, idInParent, tableToFieldTypes(alias), children)

  def sameIdEntity(alias: Alias, alias1: String, primaryKey: Keys, children: List[ChildEntity]): SameIdEntity =
    SameIdEntity(alias, alias1, primaryKey, tableToFieldTypes(alias), children)

  def oneToManyEntity(alias: Alias, alias1: String, primaryKey: Keys, parentId: Keys, children: List[ChildEntity]): OneToManyEntity =
    OneToManyEntity(alias, alias1, primaryKey, parentId, tableToFieldTypes(alias), children)
}





