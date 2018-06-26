/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.orm

import one.xingyi.core.language.AnyLanguage._

/** This applies the sql defined in FastOrmSql to the entities in a composite entity  */
object OrmStrategies extends OrmStrategies
trait OrmStrategies {
  def dropTables(implicit fastOrmSql: FastOrmSql): EntityStrategy[String] = EntityStrategy(fastOrmSql.dropTable, _ => fastOrmSql.dropTable)
  def createTables(implicit fastOrmSql: FastOrmSql): EntityStrategy[String] = EntityStrategy(fastOrmSql.createTable, _ => fastOrmSql.createOneToManyTable)

  def dropTempTables(implicit fastOrmSql: FastOrmSql): EntityStrategy[String] = EntityStrategy(fastOrmSql.dropTempTable, _ => fastOrmSql.dropTempTable)
  def createTempTables(batchDetails: BatchDetails)(implicit fastOrmSql: FastOrmSql): EntityStrategy[String] = EntityStrategy(fastOrmSql.createMainTempTable(batchDetails), fastOrmSql.createChildTempTable)
  def drainTempTables(implicit fastOrmSql: FastOrmSql): EntityStrategy[String] = EntityStrategy(fastOrmSql.drainSql, _ => fastOrmSql.drainSql)

  def insertData(implicit fastOrmSql: FastOrmSql) = EntityStrategy(fastOrmSql.insertSql, _ => fastOrmSql.insertSql)
}

case class EntityStrategy[X](mainEntity: OrmEntity => X, oneToManyEntity: OrmEntity => OneToManyEntity => X) {
  def map[T](fn: X => T) = EntityStrategy(mainEntity andThen fn, p => c => fn(oneToManyEntity(p)(c)))
  def childEntity(parentEntity: OrmEntity): PartialFunction[ChildEntity, X] = {case e: OneToManyEntity => oneToManyEntity(parentEntity)(e)}
  def walk(e: MainEntity): List[(OrmEntity, X)] = (e, mainEntity(e)) :: e.children.flatMap(walkChildren(e))
  def walkChildren(parent: OrmEntity)(child: ChildEntity): List[(OrmEntity, X)] = (child, childEntity(parent)(child)) :: child.children.flatMap(walkChildren(child))
}

/** This is the layer of abstraction that needs to be rewritten for different databases. It's just a block of sql for each operation */
trait FastOrmSql {
  import FieldType.nameAndTypeName

  def dropTable(e: OrmEntity) = s"drop table if exists ${e.tableName}"
  def dropTempTable(e: OrmEntity) = s"drop table if exists ${tempTableName(e)}"
  def tempTableName(e: OrmEntity): String = "temp_" + e.tableName

  def createTable(e: OrmEntity): String = s"create table ${e.tableName} (${e.fieldsForCreate.asString(nameAndTypeName)})"
  def createOneToManyTable(e: OneToManyEntity): String = s"create table ${e.tableName} (${e.fieldsForCreate.asString(nameAndTypeName)})"

  def createMainTempTable(batchDetails: BatchDetails)(e: OrmEntity): String =
    s"create temporary table ${tempTableName(e)} as select ${selectFields(e)} from ${e.tableName} ${e.alias} limit ${batchDetails.batchSize} offset ${batchDetails.offset}"

  def createChildTempTable(parent: OrmEntity)(e: OneToManyEntity): String =
    s"create temporary table temp_${e.tableName} as select ${e.alias}.${e.parentId.name}, ${selectFields(e)} " +
      s"from ${tempTableName(parent)} ${parent.alias},${e.tableName} ${e.alias} " +
      s"where ${parent.alias}.${parent.primaryKeyField.name} = ${e.alias}.${e.parentId.name}"

  def drainSql(e: OrmEntity): String = s"select * from ${tempTableName(e)}"

  def selectFields(e: OrmEntity): String = (e.primaryKeyField :: e.dataFields).map(f => e.alias + "." + f.name).mkString(", ")

  def insertSql(e: OrmEntity) =
    s"insert into ${e.tableName} (${e.fieldsForCreate.asString(_.name)}) values (${e.fieldsForCreate.asString(_ => "?")})"

}
object FastOrmSql {
  implicit object DefaultFastOrmSql extends FastOrmSql
}
