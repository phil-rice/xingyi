package one.xingyi.core.orm

sealed trait FieldType[T] {
  def name: String
  def typeName: String
}
object FieldType{
  def nameAndTypeName[_](ft: FieldType[_]): String = ft.name + " " + ft.typeName
}

case class StringField(name: String, typeName: String = "varchar(255)") extends FieldType[String]
case class IntField(name: String, typeName: String = "integer") extends FieldType[Int]