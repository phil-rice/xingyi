package one.xingyi.core.orm

import java.sql.PreparedStatement

import one.xingyi.core.jdbc.SetParams

trait WhereForTable extends SetParams[PreparedStatement] {
  def where(alias: String): Option[String]
}
case object NullWhereForTable extends WhereForTable {
  override def where(alias: String): Option[String] = None
  override def setParams(preparedStatement: PreparedStatement): Unit = {}
}
case class IDWhereForTable[ID](idField: FieldType[ID], id: ID) extends WhereForTable {
  def where(alias: String): Option[String] = Some(s"$alias.${idField.name}=?")
  override def setParams(preparedStatement: PreparedStatement): Unit = preparedStatement.setObject(1, id)
}
