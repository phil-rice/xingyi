package one.xingyi.core.orm


import one.xingyi.core.orm.Alias.defaultAlias
import one.xingyi.core.parserAndWriter.Parser

import scala.language.higherKinds

object AliasAndFieldType {
  def toString(list: List[AliasAndFieldType[_]]): String = list.groupBy(_.alias).toList.sortBy(_._1.tableName.tableName).map {
    case (alias, tableAndFields) => alias.tableName.tableName + ": " + tableAndFields.map(_.fieldType.prettyPrint).mkString(", ")
  }.mkString(", ")

  def apply(alias: Alias, firstField: String, fields: Seq[String]): List[AliasAndFieldType[_]] =
    (firstField :: fields.toList).map(f => AliasAndFieldType(alias, FieldType(f)))
}
object Table {
  def apply(name: String, description: String): Alias = Alias(name, description)
}
object Alias {
  def defaultAlias(tableName: String) = "A" + tableName
  //  def apply(tableName: String): Alias = Alias(TableName(tableName, ""), defaultAlias(tableName))
  def apply(tableName: String, description: String = "", alias: String = ""): Alias = Alias(TableName(tableName, description), if (alias == "") defaultAlias(tableName) else alias)
  //  def apply(tableName: TableName): Alias = Alias(tableName, "A" + tableName.tableName)
}
case class Alias(tableName: TableName, alias: String) {
  def prettyPrint = if (alias == defaultAlias(tableName.tableName)) tableName.tableName else tableName.tableName + "/" + alias
}
case class TableName(tableName: String, description: String)

case class AliasAndFieldType[T](alias: Alias, fieldType: FieldType[T]) {
  def fieldName = fieldType.name
}

case class AliasAndFieldTypes[Context, T](alias: Alias, fieldTypes: List[FieldType[_]])(implicit val tx: ValueFromMultipleAliasFields[Context, T])

trait ValueFromMultipleAliasFields[Context, T] {
  def apply(context: Context, fieldTypeToIndex: FieldTypeToIndex, fieldTypes: List[FieldType[_]]): (List[Any] => T)
}
object ValueFromMultipleAliasFields {
  implicit def valueFromMultipleTableFieldsFor[Context, T](implicit parser: Parser[T]): ValueFromMultipleAliasFields[Context, T] = new ValueFromMultipleAliasFields[Context, T] {
    override def apply(context: Context, fieldTypeToIndex: FieldTypeToIndex, fieldTypes: List[FieldType[_]]): List[Any] => T = {
      require(fieldTypes.size == 1);
      { row =>
        val i = fieldTypeToIndex.fieldTypeToIndex(fieldTypes.head)
        if (i == -1) throw new RuntimeException(s"Cannot find index for ${fieldTypes.head} in $fieldTypeToIndex")
        parser(row(i).toString)
      }
    }
  }
  implicit def valueFromMultipleTableFieldsForPlaceHolder[Context]: ValueFromMultipleAliasFields[Context, Placeholder] = new ValueFromMultipleAliasFields[Context, Placeholder] {
    override def apply(context: Context, fieldTypeToIndex: FieldTypeToIndex, fieldTypes: List[FieldType[_]]): List[Any] => Placeholder = {
      throw new RuntimeException(s"Software bug: the code should not try to create a value for this")
    }
  }
}
