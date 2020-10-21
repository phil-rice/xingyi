package one.xingyi.core.orm

import java.sql.PreparedStatement

import one.xingyi.core.jdbc.SetParams

trait WhereForTable extends SetParams[PreparedStatement] {
  def where(alias: Alias): Option[String]
}
case object NullWhereForTable extends WhereForTable {
  override def where(alias: Alias): Option[String] = None
  override def setParams(preparedStatement: PreparedStatement): Unit = {}
}
case class IDWhereForTable(keys: Keys, ids: List[Any]) extends WhereForTable {
  def whereSql(alias: String) = keys.list.map(k => s"$alias.${k.name}=?").mkString(" and ")

  def where(alias: Alias): Option[String] = Some(whereSql(alias.alias))
  override def setParams(preparedStatement: PreparedStatement): Unit =
    ids.zipWithIndex.foreach { case (id, i) => preparedStatement.setObject(i + 1, id) }
}
