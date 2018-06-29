package one.xingyi.jdbc


sealed trait FieldType {
  def digest(a: Any): String
}
object StringFieldType extends FieldType {
  override def digest(a: Any): String = a.toString
}
object IntFieldType extends FieldType {
  override def digest(a: Any): String = a.toString
}
object BlobFieldType extends FieldType {
  override def digest(a: Any): String = a.toString
}

case class FieldDefn(name: String, typeName: FieldType, digestable: Boolean)


trait ToDaoMap[T] extends (T => DaoMap)
trait FromDaoMap[T] extends (DaoMap => T)

trait TableNameFor[T]{
  def tableName: String
}
case class Schema[T]( idField: FieldDefn, otherFields: List[FieldDefn]){
  val fields = idField :: otherFields
}
