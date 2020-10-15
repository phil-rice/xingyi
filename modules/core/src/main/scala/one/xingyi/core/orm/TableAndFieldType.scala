package one.xingyi.core.orm


import one.xingyi.core.parserAndWriter.Parser

import scala.language.higherKinds

object TableAndFieldType {
  def toString(list: List[TableAndFieldType[_]]): String = list.groupBy(_.tableName).toList.sortBy(_._1.tableName).map {
    case (table, tableAndFields) => table.tableName + ": " + tableAndFields.map(_.fieldType.prettyPrint).mkString(", ")
  }.mkString(", ")

  def apply(table: TableName, firstField: String, fields: Seq[String]): List[TableAndFieldType[_]] =
    (firstField :: fields.toList).map(f => TableAndFieldType(table, FieldType(f)))
}

case class TableName(tableName: String, description: String)
case class TableAndFieldType[T](tableName: TableName, fieldType: FieldType[T]) {
  def fieldName = fieldType.name
}

case class TableAndFieldTypes[Context, T](tableName: TableName, fieldTypes: List[FieldType[_]])(implicit val tx: ValueFromMultipleTableFields[Context, T])

trait ValueFromMultipleTableFields[Context, T] {
  def apply(context: Context, fieldTypeToIndex: FieldTypeToIndex, fieldTypes: List[FieldType[_]]): (List[Any] => T)
}
object ValueFromMultipleTableFields {
  implicit def valueFromMultipleTableFieldsFor[Context, T](implicit parser: Parser[T]): ValueFromMultipleTableFields[Context, T] = new ValueFromMultipleTableFields[Context, T] {
    override def apply(context: Context, fieldTypeToIndex: FieldTypeToIndex, fieldTypes: List[FieldType[_]]): List[Any] => T = {
      require(fieldTypes.size == 1);
      { row =>
        val i = fieldTypeToIndex.fieldTypeToIndex(fieldTypes.head)
        if (i == -1) throw new RuntimeException(s"Cannot find index for ${fieldTypes.head} in $fieldTypeToIndex")
        parser(row(i).toString)
      }
    }
  }
  implicit def valueFromMultipleTableFieldsForPlaceHolder[Context]: ValueFromMultipleTableFields[Context, Placeholder] = new ValueFromMultipleTableFields[Context, Placeholder] {
    override def apply(context: Context, fieldTypeToIndex: FieldTypeToIndex, fieldTypes: List[FieldType[_]]): List[Any] => Placeholder = {
      throw new RuntimeException(s"Software bug: the code should not try to create a value for this")
    }
  }
}
