package one.xingyi.core.orm

import java.sql.PreparedStatement
import java.util.concurrent.atomic.AtomicInteger

case class WhereForTableForTest(where: String) extends WhereForTable {
  var ps: PreparedStatement = null
  val count = new AtomicInteger()
  override def where(alias: Alias): Option[String] = Some(where)
  override def setParams(preparedStatement: PreparedStatement): Unit = {ps = preparedStatement; count.incrementAndGet() }
}