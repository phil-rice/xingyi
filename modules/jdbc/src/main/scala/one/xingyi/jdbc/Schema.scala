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


trait ToDaoMap[T] {
  def main: T => DaoMap
  def child: T => DaoMap
}
trait FromDaoMap[T] extends (DaoMap => T) {
  def fromDaoHistory(t: WithHistory[DaoMap]) = WithHistory(apply(t.main), t.history.map(apply))
}

case class TableName(name: String)

case class Schema[ID, T](mainTableName: TableName, secondaryTableName: TableName, idField: FieldDefn, digestField: FieldDefn, otherFields: List[FieldDefn]) {
  val fieldsToDigest = idField :: otherFields
}
