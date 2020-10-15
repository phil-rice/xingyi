package one.xingyi.core.orm

case class Keys(val list: List[FieldType[_]]) {
  val nameString: String = list.map(_.name).mkString(",")
  def checkCanLinkTo(keys: Keys) {
    require(keys.list.size == list.size, s"Cannot link keys with $nameString to keys with ${keys.nameString} as size mismatch")
    list.zip(keys.list).foreach { case (k1, k2) => if (k1.classTag.runtimeClass != k2.classTag.runtimeClass)
      throw new RuntimeException(s"Validation. Cannot use $nameString to link to ${keys.nameString}. Fields $k1 and $k2 have different types")
    }
  }
  var map = Map[OrmEntity, KeysAndIndex]()
  def addToMap(entity: OrmEntity) = try {
    val result: KeysAndIndex = KeysAndIndex(list.map(l => (entity.fieldsForCreate.indexOf(l), l)))
    map = map + (entity -> result)
    result
  } catch {case e: Exception => throw new RuntimeException(s"Error creating index. Entity was ${entity.tableName.tableName}/${entity.alias}", e)}
  def toKeysAndIndex(entity: OrmEntity): KeysAndIndex = map.getOrElse(entity, addToMap(entity))
}

object Keys {
  def notFound[V](keys: KeysAndIndex, map: Map[Any, V], key: Any) = throw new RuntimeException(s"Cannot find key $key when the keys are $keys map is $map")
  def apply(str: String): Keys = Keys((str.split(",").map(_.trim).map(FieldType.apply)).toList)

  def zip(keys1: Keys, keys2: Keys): List[(FieldType[_], FieldType[_])] = {
    keys1.checkCanLinkTo(keys2)
    keys1.list.zip(keys2.list)
  }
}

class GetKey(indexes: List[Int]) extends Function1[List[Any], Any] {
  override def apply(oneRow: List[Any]): Any = if (indexes.size == 0) oneRow(indexes.head) else indexes.map(oneRow)
  override def toString(): String = s"GetKey(${indexes.mkString(",")})"
}

case class KeysAndIndex(list: List[(Int, FieldType[_])]) {
  list.foreach { case (int, ft) => require(int >= 0, s"Have $this") }
  val indexes = list.map(_._1)
  def prettyPrint = s"KeysAndIndex(${list.map { case (i, f) => s"$i,${f.name}" }.mkString(",")})"
  val getKey = new GetKey(indexes)
  def asPrimaryKeyAddTo[V](map: Map[Any, V], oneRow: List[Any], data: V): Map[Any, V] = map + (getKey(oneRow) -> data)
  def getFrom[V](map: Map[Any, V], oneRow: List[Any], default: (KeysAndIndex, Map[Any, V], Any) => V): V = {
    val key = getKey(oneRow)
    map.getOrElse(key, default(this, map, key))
  }

}