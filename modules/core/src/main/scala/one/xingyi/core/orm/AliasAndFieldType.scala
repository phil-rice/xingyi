package one.xingyi.core.orm


import one.xingyi.core.orm.Alias.defaultAlias
import one.xingyi.core.parserAndWriter.Parser

import scala.language.higherKinds

object AliasAndFieldType {
  def toString(list: List[AliasAndFieldType[_]]): String = list.groupBy(_.alias).toList.sortBy(_._1.table.name).map {
    case (alias, tableAndFields) => alias.table.name + ": " + tableAndFields.map(_.fieldType.prettyPrint).mkString(", ")
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
case class Alias(table: TableName, alias: String) {
  def prettyPrint = if (alias == defaultAlias(table.name)) table.name else table.name + "/" + alias
}
case class TableName(name: String, description: String)

case class AliasAndFieldType[T](alias: Alias, fieldType: FieldType[T]) {
  def fieldName = fieldType.name
}

case class AliasAndFieldTypes[Schema[_], T](alias: Alias, fieldTypes: List[FieldType[_]])(implicit val tx: ValueFromMultipleAliasFields[Schema, T])


trait GetPattern[Schema[_]] {
  def apply(s: Schema[_]): Option[String]
  def getOrException(s: Schema[_], errorContext: => String): String = apply(s).getOrElse(throw new RuntimeException(s"No pattern specified for $errorContext. s is ${s.getClass} / $s"))
}

trait GetPatternFor[Schema[_], T] {
  def apply(s: Schema[_]): Option[String]
}

trait ValueFromMultipleAliasFields[Schema[_], T] {
  def apply[Context: LinkPrefixFrom](context: Context, schema: Schema[T], fieldTypeToIndex: FieldTypeToIndex, fieldTypes: List[FieldType[_]]): (List[Any] => T)
}
object ValueFromMultipleAliasFields {
  //This is deliberately not implicit because it is complicated to override: often getting ambiguous implicit messages.
     def valueFromMultipleTableFieldsFor[Schema[_], T](implicit parser: Parser[T]): ValueFromMultipleAliasFields[Schema, T] = new ValueFromMultipleAliasFields[Schema, T] {
    override def apply[Context: LinkPrefixFrom](context: Context, schema: Schema[T], fieldTypeToIndex: FieldTypeToIndex, fieldTypes: List[FieldType[_]]): List[Any] => T = {
      require(fieldTypes.size == 1);
      { row =>
        val i = fieldTypeToIndex.fieldTypeToIndex(fieldTypes.head)
        if (i == -1) throw new RuntimeException(s"Cannot find index for ${fieldTypes.head} in $fieldTypeToIndex")
        parser(row(i).toString)
      }
    }
  }
  implicit def valueFromMultipleTableFieldsForPlaceHolder[Schema[_]]: ValueFromMultipleAliasFields[Schema, Placeholder] = new ValueFromMultipleAliasFields[Schema, Placeholder] {
    override def apply[Context: LinkPrefixFrom](context: Context, schema: Schema[Placeholder], fieldTypeToIndex: FieldTypeToIndex, fieldTypes: List[FieldType[_]]): List[Any] => Placeholder = {
      throw new RuntimeException(s"Software bug: the code should not try to create a value for this")
    }
  }
}
