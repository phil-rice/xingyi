/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.orm

import scala.language.higherKinds

trait FieldTypeToIndex {
  def fieldTypeToIndex[T](fieldType: FieldType[T]): Int
}
trait OrmEntity extends FieldTypeToIndex {
  override def fieldTypeToIndex[T](fieldType: FieldType[T]): Int = fieldsForCreate.indexOf(fieldType)
  def alias: Alias
  def tableName: TableName = alias.table
  def primaryKeyField: Keys
  def primaryKeyFieldsAndIndex: KeysAndIndex = primaryKeyField.toKeysAndIndex(this)
  def dataFields: List[FieldType[_]]
  def children: List[ChildEntity]
  def descendents: List[ChildEntity] = children ::: children.flatMap(_.descendents)
  //TODO This distinct ... what do I do about fieldtypes with same name/different types.... need to fix that
  def fieldsForCreate: List[FieldType[_]] = (dataFields ::: fieldsAddedByChildren ::: primaryKeyField.list).distinct

  def fieldsAddedByChildren: List[FieldType[_]] = children.flatMap(_.parentFields)
  protected def childrenPrettyString(indent: String) = if (children.isEmpty) "" else children.map(_.prettyPrint(indent + "  ")).mkString("{\n", "\n", s"\n$indent}")
  protected def fieldsPrettyString = {
    val data = s"data=${dataFields.map(_.name).mkString(",")}"
    val addedByChildren = if (fieldsAddedByChildren.isEmpty) "" else s"childrenAdded=${fieldsAddedByChildren.map(_.name).mkString(",")}, "
    addedByChildren + data
  }
  def prettyPrint(indent: String): String

  def dropTable(implicit fastOrmSql: FastOrmSql) = fastOrmSql.dropTable(this)
  def createTable(implicit fastOrmSql: FastOrmSql) = fastOrmSql.createTable(this)
  def dropTempTable(implicit fastOrmSql: FastOrmSql) = fastOrmSql.dropTempTable(this)
  def drainSql(implicit fastOrmSql: FastOrmSql): String = fastOrmSql.drainSql(this)
  def insertSql(implicit fastOrmSql: FastOrmSql): String = fastOrmSql.drainSql(this)
  def validate {
    val names = fieldsForCreate.map(_.name)
    if (names.toSet.size != names.size)
      throw new RuntimeException(s"You have the same name twice with a different type in ${tableName} ${alias}: " + fieldsForCreate.groupBy(x => x.name).filter(_._2.size != 1))
  }

}

sealed trait ChildEntity extends OrmEntity {
  def createTempTable(implicit fastOrmSql: FastOrmSql): OrmEntity => String
  def parentFields: List[FieldType[_]]
  def where: Option[WhereForChildTable]
}
trait SingleChild extends ChildEntity {
  def getData[X](parent: OrmEntity, childsData: Map[Any, X], parentsData: List[Any], default: (KeysAndIndex, Map[Any, X], Any) => X = Keys.notFound _): X

  def toMap[X](data: Map[OrmEntity, List[List[Any]]], fn: List[Any] => X): Map[Any, X] =
    data(this).foldLeft[Map[Any, X]](Map())((acc, list) => primaryKeyFieldsAndIndex.asPrimaryKeyAddTo(acc, list, fn(list)))
}


case class MainEntity(alias: Alias, primaryKeyField: Keys, dataFields: List[FieldType[_]], children: List[ChildEntity]) extends OrmEntity {
  private val allAliases = this.descendents.map(_.alias.alias)
  require(allAliases.size == allAliases.toSet.size, s"Have duplicate alias in ${this.descendents.map(_.alias.prettyPrint)}")
  def stream[T](batchConfig: OrmBatchConfig)(implicit ormMaker: OrmMaker[T], fastReaderOps: FastReaderDal, sqlOps: FastOrmSql): Stream[T] =
    stream(FastReader(batchConfig).apply(this), 0)

  private def stream[T](fn: Int => Stream[T], n: Int): Stream[T] = {
    val subStream: Stream[T] = fn(n)
    if (subStream.isEmpty) subStream else subStream #::: stream(fn, n + 1)
  }
  override def prettyPrint(i: String) = s"${i}MainEntity(${alias.prettyPrint}, id=${primaryKeyFieldsAndIndex.prettyPrint}, $fieldsPrettyString)${childrenPrettyString(i)}"
  def createTempTable(implicit fastOrmSql: FastOrmSql): BatchDetails => String = fastOrmSql.createMainTempTable(this)
}

case class OneToManyEntity(alias: Alias, primaryKeyField: Keys, parentId: Keys, dataFields: List[FieldType[_]], children: List[ChildEntity],where: Option[WhereForChildTable]) extends ChildEntity {
  override lazy val fieldsForCreate: List[FieldType[_]] = (super.fieldsForCreate ::: parentId.list).distinct
  val parentIdsAndIndex = parentId.toKeysAndIndex(this)
  override def parentFields: List[FieldType[_]] = List()
  override def prettyPrint(i: String) = s"${i}OneToMany(${alias.table}, id=${primaryKeyFieldsAndIndex.prettyPrint}, parent=${parentId.nameString} $fieldsPrettyString)${childrenPrettyString(i)}"
  override def createTempTable(implicit fastOrmSql: FastOrmSql) = fastOrmSql.createOneToManyTempTable(this)
  def toOneToManyMap[X](data: Map[OrmEntity, List[List[Any]]], parent: OrmEntity, fn: List[Any] => X): Map[Any, List[X]] =
    data(this).groupBy(parentIdsAndIndex.getKey).map { t => (t._1, t._2.map(fn)) }
  protected def emptyList[X](keys: KeysAndIndex, map: Map[Any, List[X]], key: Any) = Nil
  def getData[X](parent: OrmEntity, childsData: Map[Any, List[X]], parentsData: List[Any]): List[X] =
    parent.primaryKeyFieldsAndIndex.getFrom(childsData, parentsData, emptyList)
}

/** This will have zero or one entries for each item in the parent. It will be in 'step' with it... allowing cursors to advance together */
case class OneToZeroOneEntity(alias: Alias, primaryKeyField: Keys, idInParent: Keys, dataFields: List[FieldType[_]], children: List[ChildEntity],where: Option[WhereForChildTable]) extends SingleChild {
  override def parentFields: List[FieldType[_]] = idInParent.list
  override def prettyPrint(i: String) = s"${i}ManyToOne(${alias.table}, id=${primaryKeyField.nameString}, idInParent=${idInParent.nameString} $fieldsPrettyString)${childrenPrettyString(i)}"
  override def createTempTable(implicit fastOrmSql: FastOrmSql): OrmEntity => String = fastOrmSql.createOneToZeroOneEntityTempTable(this)
  def getData[X](parent: OrmEntity, childsData: Map[Any, X], parentsData: List[Any], default: (KeysAndIndex, Map[Any, X], Any) => X = Keys.notFound): X = {
    val idsInParentAndIndex = idInParent.toKeysAndIndex(parent)
    idsInParentAndIndex.getFrom[X](childsData, parentsData, default)
  }
}
/** this is typically a look up reference. It is very similar to 'oneToZeroOneEntity' except that many of the parent are likely to share the same value. Thus it won't be in sync */
case class ManyToOneEntity(alias: Alias, primaryKeyField: Keys, idInParent: Keys, dataFields: List[FieldType[_]], children: List[ChildEntity],where: Option[WhereForChildTable]) extends SingleChild {
  override def parentFields: List[FieldType[_]] = idInParent.list
  override def prettyPrint(i: String) = s"${i}ManyToOne(${alias.table}, id=${primaryKeyField.nameString}, idInParent=${idInParent.nameString} $fieldsPrettyString)${childrenPrettyString(i)}"
  override def createTempTable(implicit fastOrmSql: FastOrmSql): OrmEntity => String = fastOrmSql.createManyToOneTempTable(this)
  def getData[X](parent: OrmEntity, childsData: Map[Any, X], parentsData: List[Any], default: (KeysAndIndex, Map[Any, X], Any) => X = Keys.notFound): X = {
    val idsInParentAndIndex = idInParent.toKeysAndIndex(parent)
    idsInParentAndIndex.getFrom[X](childsData, parentsData, default)
  }
}

case class SameIdEntity(alias: Alias, primaryKeyField: Keys, dataFields: List[FieldType[_]], children: List[ChildEntity],where: Option[WhereForChildTable]) extends SingleChild {
  override def parentFields: List[FieldType[_]] = List()
  override def prettyPrint(i: String) = s"${i}SameId(${alias.table}, id=${primaryKeyField.nameString}, $fieldsPrettyString)${childrenPrettyString(i)}"
  override def createTempTable(implicit fastOrmSql: FastOrmSql): OrmEntity => String = fastOrmSql.createSameIdTempTable(this)
  def getData[X](parent: OrmEntity, childsData: Map[Any, X], parentsData: List[Any], default: (KeysAndIndex, Map[Any, X], Any) => X = Keys.notFound): X =
    parent.primaryKeyFieldsAndIndex.getFrom[X](childsData, parentsData, default)
}