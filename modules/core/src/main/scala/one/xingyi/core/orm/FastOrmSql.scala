/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.orm

import one.xingyi.core.language.AnyLanguage._

/** This applies the sql defined in FastOrmSql to the entities in a composite entity */
object OrmStrategies extends OrmStrategies
trait OrmStrategies {
  //TODO Redo with type classes
  def dropTables(implicit fastOrmSql: FastOrmSql): EntityStrategy[String] = EntityStrategy(_.dropTable)
  def createTables(implicit fastOrmSql: FastOrmSql): EntityStrategy[String] = EntityStrategy(_.createTable)

  def dropTempTables(implicit fastOrmSql: FastOrmSql): EntityStrategy[String] = EntityStrategy(_.dropTempTable)
  def createTempTables(batchDetails: BatchDetails)(implicit fastOrmSql: FastOrmSql): EntityStrategy[String] =
    EntityStrategy(_.createTempTable.apply(batchDetails), parent => child => child.createTempTable.apply(parent))
  def drainTempTables(implicit fastOrmSql: FastOrmSql): EntityStrategy[String] = EntityStrategy(_.drainSql)
  def insertData(implicit fastOrmSql: FastOrmSql) = EntityStrategy(fastOrmSql.insertSql)
}

object EntityStrategy {
  def apply[X](ormEntityFn: OrmEntity => X): EntityStrategy[X] = EntityStrategy(ormEntityFn, _ => ormEntityFn)
}
case class EntityStrategy[X](mainEntityFn: MainEntity => X, childFn: OrmEntity => ChildEntity => X) {
  def map[T](fn: X => T): EntityStrategy[T] = EntityStrategy(mainEntityFn andThen fn, p => c => fn(childFn(p)(c)))
  def walk(e: MainEntity): List[(OrmEntity, X)] = (e, mainEntityFn(e)) :: e.children.flatMap(walkChildren(e))
  private def walkChildren(parent: OrmEntity)(child: ChildEntity): List[(OrmEntity, X)] = (child, childFn(parent)(child)) :: child.children.flatMap(walkChildren(child))
}

/** This is the layer of abstraction that needs to be rewritten for different databases. It's just a block of sql for each operation */
trait FastOrmSql {
  import FieldType.nameAndTypeName

  def dropTable(e: OrmEntity) = s"drop table if exists ${e.tableName.name}"
  def dropTempTable(e: OrmEntity) = s"drop table if exists ${tempTableName(e)}"
  def tempTableName(e: OrmEntity): String = "temp_" + e.tableName.name

  def createTable(e: OrmEntity): String = s"create table ${e.tableName.name} (${e.fieldsForCreate.asString(nameAndTypeName(_))})"

  def createMainTempTable(e: OrmEntity)(batchDetails: BatchDetails): String =
    s"create temporary table ${tempTableName(e)} as " +
      s"select ${selectFields(e)} from ${tableAlias(e)} " +
      batchDetails.whereForTable.where(e.alias).fold("")(w => s"where $w ") +
      s"order by ${selectKey(e.alias, e.primaryKeyField)} " +
      s"limit ${batchDetails.batchSize} offset ${batchDetails.offset}"

  def addWhere(parent: OrmEntity, e: ChildEntity) = e.where.flatMap(w => w.where(parent.alias, e.alias)).map(w => s"and $w ").getOrElse("")
  def createOneToManyTempTable(e: OneToManyEntity)(parent: OrmEntity): String =
    s"create temporary table ${tempTableName(e)} as " +
      s"select ${selectFields(e)} " +
      s"from ${tempAlias(parent)},${tableAlias(e)} " +
      s"where ${whereKey(parent.alias, parent.primaryKeyField, e.alias, e.parentId)} " + addWhere(parent, e) +
      s"order by ${selectKey(e.alias, e.parentId)},${selectKey(e.alias, e.primaryKeyField)} "

  def createManyToOneTempTable(e: ManyToOneEntity)(parent: OrmEntity): String =
    s"create temporary table temp_${e.alias.table.name} as " +
      s"select ${selectFields(e)} " +
      s"from ${tempAlias(parent)},${tableAlias(e)} " +
      s"where ${whereKey(parent.alias, e.idInParent, e.alias, e.primaryKeyField)} " + addWhere(parent, e) +
      s"order by ${selectKey(parent.alias, parent.primaryKeyField)} "

  def createOneToZeroOneEntityTempTable(e: OneToZeroOneEntity)(parent: OrmEntity): String =
    s"create temporary table temp_${e.alias.table.name} as " +
      s"select DISTINCT  ${selectFields(e)} " +
      s"from ${tempAlias(parent)},${tableAlias(e)} " +
      s"where ${whereKey(parent.alias, e.idInParent, e.alias, e.primaryKeyField)} " + addWhere(parent, e) +
      s"order by ${selectKey(parent.alias, parent.primaryKeyField)} "

  def createSameIdTempTable(e: SameIdEntity)(parent: OrmEntity): String =
    s"create temporary table temp_${e.alias.table.name} as " +
      s"select DISTINCT  ${selectFields(e)} " +
      s"from ${tempAlias(parent)},${tableAlias(e)} " +
      s"where ${whereKey(parent.alias, parent.primaryKeyField, e.alias, e.primaryKeyField)} " + addWhere(parent, e) +
      s"order by ${selectKey(e.alias, e.primaryKeyField)} "


  def drainSql(e: OrmEntity): String = s"select * from ${tempTableName(e)}"

  def tableAlias(e: OrmEntity): String = e.alias.table.name + " " + e.alias.alias
  def tempAlias(e: OrmEntity): String = tempTableName(e) + " " + e.alias.alias
  def selectKey(alias: Alias, keys: Keys): String = keys.list.map(k => alias.alias + "." + k.name).mkString(", ")
  def whereKey(alias1: Alias, keys1: Keys, alias2: Alias, keys2: Keys): String =
    Keys.zip(keys1, keys2).map { case (k1, k2) => s"${alias1.alias}.${k1.name} = ${alias2.alias}.${k2.name}" }.mkString(" and ")
  def selectFields(e: OrmEntity): String = (e.fieldsForCreate).map(f => e.alias.alias + "." + f.name).mkString(", ")

  def insertSql(e: OrmEntity) =
    s"insert into ${e.tableName.name} (${e.fieldsForCreate.asString(_.name)}) values (${e.fieldsForCreate.asString(_ => "?")})"

}
object FastOrmSql {
  implicit object DefaultFastOrmSql extends FastOrmSql
}
