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

  def dropTable(e: OrmEntity) = s"drop table if exists ${e.tableName}"
  def dropTempTable(e: OrmEntity) = s"drop table if exists ${tempTableName(e)}"
  def tempTableName(e: OrmEntity): String = "temp_" + e.tableName

  def createTable(e: OrmEntity): String = s"create table ${e.tableName} (${e.fieldsForCreate.asString(nameAndTypeName(_))})"

  def createMainTempTable(e: OrmEntity)(batchDetails: BatchDetails): String =
    s"create temporary table ${tempTableName(e)} as select ${selectFields(e)} from ${e.tableName} ${e.alias} limit ${batchDetails.batchSize} offset ${batchDetails.offset}"

  def createOneToManyTempTable(e: OneToManyEntity)(parent: OrmEntity): String =
    s"create temporary table temp_${e.tableName} as select ${e.alias}.${e.parentId.name}, ${selectFields(e)} " +
      s"from ${tempTableName(parent)} ${parent.alias},${e.tableName} ${e.alias} " +
      s"where ${parent.alias}.${parent.primaryKeyField.name} = ${e.alias}.${e.parentId.name}"

  def createManyToOneTempTable(e: ManyToOneEntity)(parent: OrmEntity): String =
    s"create temporary table temp_${e.tableName} as select DISTINCT  ${selectFields(e)} " + //TODO Why do we have distinct here
      s"from ${tempTableName(parent)} ${parent.alias},${e.tableName} ${e.alias} " +
      s"where ${parent.alias}.${e.idInParent.name} = ${e.alias}.${e.primaryKeyField.name}"

  def createSameIdTempTable(e: SameIdEntity)(parent: OrmEntity): String =
    s"create temporary table temp_${e.tableName} as select DISTINCT  ${selectFields(e)} " +
      s"from ${tempTableName(parent)} ${parent.alias},${e.tableName} ${e.alias} " +
      s"where ${parent.alias}.${parent.primaryKeyField.name} = ${e.alias}.${e.primaryKeyField.name}"


  def drainSql(e: OrmEntity): String = s"select * from ${tempTableName(e)}"

  def selectFields(e: OrmEntity): String = (e.primaryKeyField :: e.fieldsAddedByChildren ::: e.dataFields).map(f => e.alias + "." + f.name).mkString(", ")

  def insertSql(e: OrmEntity) =
    s"insert into ${e.tableName} (${e.fieldsForCreate.asString(_.name)}) values (${e.fieldsForCreate.asString(_ => "?")})"

}
object FastOrmSql {
  implicit object DefaultFastOrmSql extends FastOrmSql
}
