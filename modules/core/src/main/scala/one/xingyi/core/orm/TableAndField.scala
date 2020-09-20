package one.xingyi.core.orm

object TableAndField {
  def toString(list: List[TableAndField]): String = list.groupBy(_.tableName).toList.sortBy(_._1.tableName).map {
    case (table, tableAndFields) => table.tableName + ": " + tableAndFields.map(_.fieldName).mkString(", ")
  }.mkString(", ")

  def apply(table: TableName, firstField: String, fields: Seq[String]): List[TableAndField] = (firstField :: fields.toList).map(f => TableAndField(table, f))


}

case class TableName(tableName: String, description: String)
case class TableAndField(tableName: TableName, fieldName: String)