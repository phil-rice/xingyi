package one.xingyi.core.orm

import java.sql.PreparedStatement
import java.util.concurrent.atomic.AtomicInteger

case class WhereForTableForTest(where: String) extends WhereForTable {
  var ps: PreparedStatement = null
  val count = new AtomicInteger()
  override def where(alias: Alias): Option[String] = Some(s"${alias.alias}/$where")
  override def setParams(preparedStatement: PreparedStatement): Unit = {ps = preparedStatement; count.incrementAndGet() }
}

case class WhereForChildTableForTest(where: String) extends WhereForChildTable {
  override def where(parentAlias: Alias, alias: Alias): Option[String] = Some(s"${parentAlias.alias}/${alias.alias}/$where")
}