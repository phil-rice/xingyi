package one.xingyi.core.orm

import java.io.{ByteArrayOutputStream, OutputStream}

import one.xingyi.core.orm.SchemaMapKey._
import one.xingyi.core.strings.Strings

import scala.language.higherKinds

//OK This code is imperative, and mutable...
// This is because it is about speed: this is about getting data out of the database, it's done a lot and we don't want a ton of garbage collection
//At the moment we make all the items afresh, but they could easily (and probably should be) object pooled as we try for more speed

trait AsDebugString[T] {def apply(t: T): String}
object AsDebugString {
  implicit def asDebugString[T]: AsDebugString[T] = new AsDebugString[T] {
    override def apply(t: T): String = t.toString
  }
}
object ChildArity {
  def prettyPrintOneArrayItem[T](prefix: String, key: NumericKey[T], item: Any)(implicit asDebugString: AsDebugString[T]) = {
    //    println(s"got to print print and the key is $key")
    //    println(s"   key .t is ${key.t}")
    //    println(s"   asDebugString is $asDebugString")
    val itemString = if (item == null) "null" else Strings.escapeJson(item.toString)
    s"$prefix  = $itemString ${asDebugString(key.t)}"
  }
}
sealed trait ChildArity {
  def prettyPrintArray[T: AsDebugString](prefix: String, key: NumericKey[T], item: Any, strict: Boolean): List[String]
  protected[orm] def optionallyCreateArray(a: Array[Any], i: Int, size: Int): Array[Any]
}
case object NoChildren extends ChildArity {
  override protected[orm] def optionallyCreateArray(a: Array[Any], i: Int, size: Int) = null //ouch. This is actually needed to get rid of lots of garbage collection. But *sigh*
  //  override protected[objectMap] def put[T](key: NumericKey[T], a: Array[Any], i: Int, data: Any): Unit = a(i) = data
  override def prettyPrintArray[T: AsDebugString](prefix: String, key: NumericKey[T], item: Any, strict: Boolean): List[String] =
    List(ChildArity.prettyPrintOneArrayItem(prefix, key, item))
}
case object OneChild extends ChildArity {
  override protected[orm] def optionallyCreateArray(a: Array[Any], i: Int, size: Int) = {
    val result = new Array[Any](size);
    a(i) = result;
    result
  }
  override def prettyPrintArray[T: AsDebugString](prefix: String, key: NumericKey[T], item: Any, strict: Boolean): List[String] = {
    require(item.isInstanceOf[Array[_]], s"expected array but was $item")
    prefix + "/OneChild" :: key.children.printArray(prefix, item.asInstanceOf[Array[Any]], strict)
  }
}
case object ManyChildren extends ChildArity {
  override protected[orm] def optionallyCreateArray(a: Array[Any], i: Int, size: Int) = {
    a(i) = List()
    null
  }
  override def prettyPrintArray[T: AsDebugString](prefix: String, key: NumericKey[T], item: Any, strict: Boolean): List[String] = {
    require(item.isInstanceOf[List[_]])
    val list = item.asInstanceOf[List[Array[Any]]]
    s"$prefix/Many(${list.size})" :: list.zipWithIndex.flatMap { case (a, index) =>
      val newPrefix = prefix + s"[$index]"
      key.children.printArray(newPrefix, a, strict)
    }
  }
}

sealed abstract class ChildrenInSchema[T](val arity: ChildArity) {def children: List[T]}
case class Zero[T]() extends ChildrenInSchema[T](NoChildren) {def children: List[T] = Nil}
case class AlwaysOne[T](children: List[T]) extends ChildrenInSchema[T](OneChild)
case class ZeroOrMore[T](children: List[T]) extends ChildrenInSchema[T](ManyChildren)

//THere is a schema of type Schema. Schema is not the data, is the structure of the data
trait SchemaMapKey[Schema] {
  def childKey(t: Schema): String // The main object might not have a key, but the children will
  def children(t: Schema): ChildrenInSchema[Schema]
}

object SchemaMapKey {
  implicit class SchemaMapKeyOps[S](s: S)(implicit k: SchemaMapKey[S]) {
    def key: String = k.childKey(s)
    def children: ChildrenInSchema[S] = k.children(s)
  }
}

/** The data in the database is based on tables
 * The data we want in our json or objects is based on an objectgraph
 * There has to be a mapping between the data in the database and the object graph: this is it
 *
 * Example: We have a schema:
 * main:
 * key1  table1/field1
 * key2  table2/field1
 * child:
 * childKey1 table1/field2
 * As can be seen there isn't a simple mapping
 *
 * This heavily uses type classes, so you can use your own (perhaps existing) schema object.
 * SchemaMapKey[Schema] -> This is how we know what keys exist and what children exist.
 *
 * Utility
 * putJson: Very useful if you are using APIs because it writes to an outputstream and is much less garbage/memory intensive than turning to a string first
 * toJson: Nicer interface than putJson
 *
 * Debugging:
 * prettyPrint is really helpful for understanding the schema. It is intended for use in tests
 * printArray prints the array of data retrieved from FastOrm in a way helpful for tests
 *
 * */
case class OrmKeys[Schema](list: List[NumericKey[Schema]]) {
  def allKeys: List[NumericKey[Schema]] = list.flatMap { key => key :: key.children.allKeys }
  def findForT(t: Schema) = allKeys.find(_.t == t)
  def prettyPrint(indent: String) = list.map(_.prettyPrint(indent)).mkString("\n")
  def printArray(prefix: String, a: Array[Any], strict: Boolean = true)(implicit asDebugString: AsDebugString[Schema]): List[String] = {
    require(!strict || a.length == size, s"Array size is ${a.length} keys size is $size")
    list.zip(a).flatMap { case (key, item) => key.printArray(prefix, item, strict) }
  }
  def toJson(ar: Array[Any]): String = {
    val stream = new ByteArrayOutputStream()
    putJson(ar, stream)
    stream.toString()
  }
  def putJson(ar: Array[Any], outputStream: OutputStream) {
    def putRawString(s: String) = {
      var i = 0
      def escape(c: Char): Unit = {outputStream.write('\\'); outputStream.write(c) }
      while (i < s.length) {
        s.charAt(i) match {
          case ('\\') => escape('\\')
          case ('\b') => escape('b')
          case ('\f') => escape('f')
          case ('\n') => escape('n')
          case ('\r') => escape('r')
          case ('\t') => escape('t')
          case ('"') => escape('"')
          case c => outputStream.write(c)
        }
        i += 1
      }
    }
    def putString(s: String): Unit = {
      outputStream.write('"')
      putRawString(s)
      outputStream.write('"')

    }
    var i = 0
    outputStream.write('{')
    while (i < ar.length) {
      if (i != 0) outputStream.write(',')
      putString(list(i).key)
      outputStream.write(':')
      ar(i) match {
        case s: String => putString(s)
        case i: Integer => putRawString(i.toString)
        case d: Double => putRawString(d.toString)
        case a: Array[Any] => list(i).children.putJson(a, outputStream)
        case (head: Array[Any]) :: tail =>
          outputStream.write('[')
          tail.reverse.foreach { case a: Array[Any] =>
            list(i).children.putJson(a, outputStream)
            outputStream.write(',')
          }
          list(i).children.putJson(head, outputStream)
          outputStream.write(']')
        case Nil => outputStream.write('['); outputStream.write(']')
        case null => putRawString("null")
      }
      i += 1
    }
    outputStream.write('}')
  }

  val size: Int = list.size
  def makeAndSetupArray: Array[Any] = {
    val a = new Array[Any](size)
    setup(a)
    a
  }
  def setup(array: Array[Any]) {
    var i = 0
    while (i < size) {
      val thisKey = list(i)
      val a = thisKey.arity.optionallyCreateArray(array, i, thisKey.children.size)
      if (a != null) thisKey.children.setup(a)
      i = i + 1
    }
  }

  def asArray(a: Any, msg: => String): Array[Any] = a match {
    case ar: Array[Any] => ar
    case list: List[_] => list.headOption.getOrElse(throw new RuntimeException(s"Trying to access an empty list. Did you need to call 'next'?")).asInstanceOf[Array[Any]]
    case _ => throw new IllegalArgumentException(msg)
  }
  def asList(a: Any, msg: => String): List[Array[Any]] = a match {
    case list: List[_] => list.asInstanceOf[List[Array[Any]]]
    case _: Array[Any] => throw new IllegalArgumentException(msg + "(array)")
    case _ => throw new IllegalArgumentException(msg)
  }

  def findLastArray(path: Array[Int], array: Array[Any]): Array[Any] = {
    var a = array
    var pathIndex = 0
    while (pathIndex < path.size) {
      val i = path(pathIndex)
      a = asArray(a(i), s"following path ${path.toList.mkString(",")} and ran out Index is $pathIndex index is $i and a is ${a.toList}")
      pathIndex += 1
    }
    asArray(a, s"At end of the path $path")
  }
  def put(path: Array[Int], index: Int, array: Array[Any], data: Any): Unit = {
    val ar = findLastArray(path, array)
    ar(index) match {
      case a: Array[_] => throw new IllegalStateException(s"You cannot change an array with put. Path is ${path.toList} index is ${index}")
      case a: List[_] => throw new IllegalStateException(s"You cannot change a list with put. Path is ${path.toList} index is ${index}")
      case _ => ar(index) = data
    }
  }

  def findLastArrayForList(path: Array[Int], array: Array[Any]) = {
    require(path.size > 0)
    var a = array
    var pathIndex = 0
    while (pathIndex < path.size - 1) {
      val i = path(pathIndex)
      a = asArray(a(i), s"following path ${path.toList.mkString(",")} and ran out Index is $pathIndex index is $i and a is $a")
      pathIndex += 1
    }
    a
  }

  def debugPrint(path: Array[Int], array: Array[Any]): Unit = {
    def printMe(a: Any) = {if (a.isInstanceOf[Array[_]]) a.asInstanceOf[Array[_]].mkString(",") else "" + a }
    println("path: " + path.mkString(","))
    var pathIndex = 0
    var a = array
    while (pathIndex < path.length) {
      val i = path(pathIndex)
      println(s"  array has length ${a.length} and this Path index is $i")
      val obj = a(i)
      println(s"   item at $i is ${printMe(obj)}")
      a = asArray(obj, s"in debug item is $i path is ${path.mkString(",")} obj was $obj")
      pathIndex += 1
    }
  }
  def findKeyForPath(path: Array[Int]): NumericKey[Schema] = {
    var pathIndex = 0
    var keys = this
    while (pathIndex < path.length - 1) {
      val i = path(pathIndex)
      keys = keys.list(i).children
      pathIndex += 1
    }
    keys.list(path(path.length - 1))

  }
  def next(path: Array[Int], array: Array[Any]): Unit = {
    val lastArray = findLastArrayForList(path, array)
    val list = asList(lastArray(path(path.length - 1)), s"At end of the path $path")
    val ar = findKeyForPath(path).children.makeAndSetupArray
    lastArray(path(path.length - 1)) = ar :: list
  }
}
case class NumericKey[T](path: List[Int], index: Int, key: String, arity: ChildArity, children: OrmKeys[T], t: T) {
  def printArray(prefix: String, item: Any, strict: Boolean)(implicit asDebugString: AsDebugString[T]): List[String] = {
    val newPrefix = if (prefix.isEmpty) index.toString else prefix + "." + index
    arity.prettyPrintArray(newPrefix, this, item, strict)
  }

  def prettyPrint(indent: String): String = {
    val prefix = s"${indent}$key (${path.mkString(",")}),$index $arity"
    if (children.size > 0) prefix + "\n" + children.prettyPrint(indent + " ") else prefix
  }

  def size = children.size
}

object OrmKeys {
  def apply[Schema: SchemaMapKey](t: Schema): OrmKeys[Schema] = recurse(List(), t.children.children)
  def apply[Schema: SchemaMapKey](ts: List[Schema]): OrmKeys[Schema] = recurse(List(), ts)
  def recurse[Schema](parents: List[Int], ts: List[Schema])(implicit schemaMapKey: SchemaMapKey[Schema]): OrmKeys[Schema] =
    OrmKeys(ts.zipWithIndex.map { case (t, index) =>
      val children: ChildrenInSchema[Schema] = schemaMapKey.children(t)
      NumericKey(parents, index, schemaMapKey.childKey(t), children.arity, recurse(parents :+ index, children.children), t)
    })
}

class NumericKeyPopulator[T](numericKeys: OrmKeys[T], tablesAndFieldsAndPaths: TablesAndFieldsAndPaths, mainEntity: MainEntity, map: Map[OneToManyEntity, NumericKey[_]])(implicit findOrmEntityAndField: FindOrmEntityAndField[T])
  extends ((Array[Any], OrmEntity, List[Any]) => Array[Any]) {
  val nextMap = map.map { case (k, v) => (k.alias, (v.path :+ v.index).toArray) }

  //  val tablesAndFieldsAndPaths: TablesAndFieldsAndPaths = EntityAndPath(numericKeys)
  val aliasToOrmGetters: Map[String, OrmGettersForThisRowAndPath] = (mainEntity :: mainEntity.descendents).map { entity =>
    (entity.alias, tablesAndFieldsAndPaths.getOrmGettersAndPath(entity.tableName).toForThisRow(entity.fieldsForCreate.map(_.name)))
  }.toMap

  def apply(ar: Array[Any], entity: OrmEntity, oneRow: List[Any]): Array[Any] = {
    nextMap.get(entity.alias).foreach(path => numericKeys.next(path, ar))
    val fieldsAndPath = aliasToOrmGetters(entity.alias)
    val ormGetters: Array[OrmValueGetterForARow[_]] = fieldsAndPath.ormValueGetters
    val paths = fieldsAndPath.path
    val indicies = fieldsAndPath.indicies
    var i = 0
    while (i < ormGetters.length) {
      val d = ormGetters(i) apply (oneRow.toArray)
      numericKeys.put(paths(i), indicies(i), ar, d)
      i += 1
    }
    ar
  }

}