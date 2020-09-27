package one.xingyi.core.orm

object TableAndFieldType {
  def toString(list: List[TableAndFieldType[_]]): String = list.groupBy(_.tableName).toList.sortBy(_._1.tableName).map {
    case (table, tableAndFields) => table.tableName + ": " + tableAndFields.map(_.fieldType.prettyPrint).mkString(", ")
  }.mkString(", ")

  def apply(table: TableName, firstField: String, fields: Seq[String]): List[TableAndFieldType[_]] =
    (firstField :: fields.toList).map(f => TableAndFieldType(table, FieldType(f)))


}

case class TableName(tableName: String, description: String)
case class TableAndFieldType[T](tableName: TableName, fieldType: FieldType[T]){
  def fieldName=fieldType.name
}

