package one.xingyi.core.orm

import java.sql.PreparedStatement

import one.xingyi.core.jdbc.SetParams

trait WhereForChildTable  {
  def where(parentlias: Alias, alias: Alias): Option[String]
}

