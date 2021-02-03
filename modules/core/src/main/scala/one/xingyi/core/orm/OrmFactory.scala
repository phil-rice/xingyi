package one.xingyi.core.orm

import scala.language.higherKinds

object OrmFactory {
  def apply[Schema[_]](schema: Schema[_])(implicit toTableAndFieldTypes: ToAliasAndFieldTypes[Schema], schemaMapKey: SchemaMapKey[Schema]): OrmFactory[Schema] =
    new OrmFactoryImpl(schema)
}

trait OrmFactory[Schema[_]] {
  def mainEntity(alias: Alias, primaryKey: Keys, children: List[ChildEntity]): MainEntity
  def manyToOneEntity(alias: Alias, primaryKey: Keys, idInParent: Keys, children: List[ChildEntity], where: Option[WhereForTable] = None): ManyToOneEntity
  def sameIdEntity(alias: Alias, primaryKey: Keys, children: List[ChildEntity], where: Option[WhereForTable] = None): SameIdEntity
  def oneToManyEntity(alias: Alias, primaryKey: Keys, parentId: Keys, children: List[ChildEntity], where: Option[WhereForTable] = None): OneToManyEntity
}

class OrmFactoryImpl[Schema[_]](schema: Schema[_])(implicit toTableAndFieldTypes: ToAliasAndFieldTypes[Schema], schemaMapKey: SchemaMapKey[Schema]) extends OrmFactory[Schema] {
  private val tableToFieldTypes: Map[Alias, List[FieldType[_]]] =
    schemaMapKey.descendants(schema).
      flatMap { s: Schema[_] => toTableAndFieldTypes.apply(s).map(tf => tf.alias -> tf.fieldTypes) }.
      groupBy(_._1).map { case (alias, list) => alias -> list.flatMap(_._2) }
  def get(alias: Alias) = tableToFieldTypes.getOrElse(alias, Nil)
  def mainEntity(alias: Alias, primaryKey: Keys, children: List[ChildEntity]): MainEntity =
    MainEntity(alias, primaryKey, get(alias), children)

  def manyToOneEntity(alias: Alias, primaryKey: Keys, idInParent: Keys, children: List[ChildEntity], where: Option[WhereForTable] = None): ManyToOneEntity =
    ManyToOneEntity(alias, primaryKey, idInParent, get(alias), children, where)

  def sameIdEntity(alias: Alias, primaryKey: Keys, children: List[ChildEntity], where: Option[WhereForTable] = None): SameIdEntity =
    SameIdEntity(alias, primaryKey, get(alias), children, where)

  def oneToManyEntity(alias: Alias, primaryKey: Keys, parentId: Keys, children: List[ChildEntity], where: Option[WhereForTable] = None): OneToManyEntity =
    OneToManyEntity(alias, primaryKey, parentId, get(alias), children, where)
}





