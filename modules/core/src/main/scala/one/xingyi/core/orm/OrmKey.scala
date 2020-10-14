package one.xingyi.core.orm

import java.io.{ByteArrayOutputStream, OutputStream}

import one.xingyi.core.aggregate.HasChildrenForHolder
import one.xingyi.core.logging.LoggingAdapter
import one.xingyi.core.orm.SchemaMapKey._
import one.xingyi.core.strings.Strings

import scala.annotation.implicitNotFound
import scala.collection.immutable.List
import scala.language.higherKinds

//OK This code is imperative, and mutable...
// This is because it is about speed: this is about getting data out of the database, it's done a lot and we don't want a ton of garbage collection
//At the moment we make all the items afresh, but they could easily (and probably should be) object pooled as we try for more speed

@implicitNotFound("This is something that you should write for the schema. If you want to use a simple one there is an example in the object AsDebugString")
trait AsDebugString[Schema[_]] {
  def apply[T](t: Schema[T]): String
  def data(t: Any): String
}
object AsDebugString {
  def asDebugString[Schema[_]]: AsDebugString[Schema] = new AsDebugString[Schema] {
    override def apply[T](t: Schema[T]): String = t.toString
    override def data(t: Any): String = t.toString
  }
}
object ChildArity {
  def prettyPrintOneArrayItem[Schema[_], T](prefix: String, key: OrmKey[Schema, T], item: Any)(implicit asDebugString: AsDebugString[Schema]) = {
    //    println(s"got to print print and the key is $key")
    //    println(s"   key .t is ${key.t}")
    //    println(s"   asDebugString is $asDebugString")
    val itemString = if (item == null) "null" else Strings.escapeJson(asDebugString.data(item))
    s"$prefix  = $itemString ${asDebugString(key.t)}"
  }
}
sealed trait ChildArity {
  def prettyPrintArray[Schema[_] : AsDebugString, T](prefix: String, key: OrmKey[Schema, T], item: Any, strict: Boolean): List[String]
  protected[orm] def optionallyCreateArray(a: Array[Any], i: Int, size: Int): Array[Any]
}
case object NoChildren extends ChildArity {
  override protected[orm] def optionallyCreateArray(a: Array[Any], i: Int, size: Int) = null //ouch. This is actually needed to get rid of lots of garbage collection. But *sigh*
  //  override protected[objectMap] def put[T](key: NumericKey[T], a: Array[Any], i: Int, data: Any): Unit = a(i) = data
  override def prettyPrintArray[Schema[_] : AsDebugString, T](prefix: String, key: OrmKey[Schema, T], item: Any, strict: Boolean): List[String] =
    List(ChildArity.prettyPrintOneArrayItem(prefix, key, item))
}
case object OneChild extends ChildArity {
  override protected[orm] def optionallyCreateArray(a: Array[Any], i: Int, size: Int) = {
    val result = new Array[Any](size);
    a(i) = result;
    result
  }
  override def prettyPrintArray[Schema[_] : AsDebugString, T](prefix: String, key: OrmKey[Schema, T], item: Any, strict: Boolean): List[String] = {
    require(item.isInstanceOf[Array[_]], s"expected array but was $item")
    prefix + "/OneChild" :: key.children.printArray(prefix, item.asInstanceOf[Array[Any]], strict)
  }
}
case object ManyChildren extends ChildArity {
  override protected[orm] def optionallyCreateArray(a: Array[Any], i: Int, size: Int) = {
    a(i) = List()
    null
  }
  override def prettyPrintArray[Schema[_] : AsDebugString, T](prefix: String, key: OrmKey[Schema, T], item: Any, strict: Boolean): List[String] = {
    require(item.isInstanceOf[List[_]])
    val list = item.asInstanceOf[List[Array[Any]]]
    s"$prefix/Many(${list.size})" :: list.zipWithIndex.flatMap { case (a, index) =>
      val newPrefix = prefix + s"[$index]"
      key.children.printArray(newPrefix, a, strict)
    }
  }
}

sealed abstract class ChildrenInSchema[Schema[_]](val arity: ChildArity) {def children: List[Schema[_]]}
case class Zero[Schema[_]]() extends ChildrenInSchema[Schema](NoChildren) {def children: List[Schema[_]] = Nil}
case class AlwaysOne[Schema[_]](children: List[Schema[_]]) extends ChildrenInSchema[Schema](OneChild)
case class ZeroOrMore[Schema[_]](children: List[Schema[_]]) extends ChildrenInSchema[Schema](ManyChildren)

trait Placeholder

/** THere is a schema of type Schema. Schema is not the data, is the structure of the data
 *
 * The type parameter is the type of the data in the field that the schema is pointing to.
 * This is used for things like 'getting from database' and 'putting in json'.
 *
 * Because schemas hold other schemas, and I am not sure how to handle the schema object itself from a [T] perspective
 * there is a fake T called Placeholder. */
trait SchemaMapKey[Schema[_]] {
  def childKey[T](t: Schema[T]): String // The main object might not have a key, but the children will
  def children[T](t: Schema[T]): ChildrenInSchema[Schema]
  def descendants[T](t: Schema[T]): List[Schema[_]] = {
    val c: List[Schema[_]] = children(t).children
    c ::: c.flatMap(descendants(_))
  }
}

object SchemaMapKey {
  implicit class SchemaMapKeyOps[S[_], T](s: S[T])(implicit k: SchemaMapKey[S]) {
    def key: String = k.childKey(s)
    def children: ChildrenInSchema[S] = k.children(s)
    def descendants: List[S[_]] = k.descendants(s)
  }
}


trait JsonToStream[Context, F[_], T] {
  def put(c: Context, f: F[T], t: Any, stream: OutputStream)
}
object JsonToStream {
  def putUnescaped(outputStream: OutputStream, s: String) {outputStream.write(s.getBytes) }
  def putEscaped(s: String, stream: OutputStream) {
    var i = 0
    def escape(c: Char): Unit = {stream.write('\\'); stream.write(c) }
    while (i < s.length) {
      s.charAt(i) match {
        case ('\\') => escape('\\')
        case ('\b') => escape('b')
        case ('\f') => escape('f')
        case ('\n') => escape('n')
        case ('\r') => escape('r')
        case ('\t') => escape('t')
        case ('"') => escape('"')
        case c => stream.write(c)
      }
      i += 1
    }
  }
  def putEscapedWithQuotes(s: String, stream: OutputStream) {
    stream.write('"')
    putEscaped(s, stream)
    stream.write('"')
  }

  def asStringWithQuotes[Context, F[_], T]: JsonToStream[Context, F, T] = (c: Context, f: F[T], s: Any, stream: OutputStream) => putEscapedWithQuotes(s.toString, stream)
  def asStringFnWithQuotes[Context, F[_], T](fn: Any => String): JsonToStream[Context, F, T] = (c: Context, f: F[T], s: Any, stream: OutputStream) => putEscapedWithQuotes(fn(s), stream)

  implicit def StringJsonToStream[Context, F[_]]: JsonToStream[Context, F, String] = asStringWithQuotes //(s: Any, stream: OutputStream) => putEscapedWithQuotes(s.toString, stream)
  implicit def IntJsonToStream[Context, F[_]]: JsonToStream[Context, F, Int] = (c: Context, f: F[Int], t: Any, stream: OutputStream) => putUnescaped(stream, t.toString)
  implicit def DoubleJsonToStream[Context, F[_]]: JsonToStream[Context, F, Double] = (c: Context, f: F[Double], t: Any, stream: OutputStream) => putUnescaped(stream, t.toString)
  implicit def PlaceholderJsonToStream[Context, F[_]]: JsonToStream[Context, F, Placeholder] = (c: Context, f: F[Placeholder], t: Any, stream: OutputStream) => throw new RuntimeException("Should not attempt to write a placeholder to the output stream")
}

trait OrmKeysToJson[Context, Schema[_]] {
  def putJson(c: Context, keys: OrmKeys[Schema], ar: Array[Any], outputStream: OutputStream)
}
trait JsonToStreamFor[Context, Schema[_]] {
  def putToJson[T](c: Context, s: Schema[T]): JsonToStream[Context, Schema, T]
}
//trait JsonToStreamForLink[Schema[_]] extends SendJsonToStream[Schema]

object OrmKeysToJson {
  implicit def ormKeysToJsonViaArrayFirst[Context, Schema[_]](implicit jsonToStreamFor: JsonToStreamFor[Context, Schema]): OrmKeysToJson[Context, Schema] = new OrmKeysToJson[Context, Schema] {
    override def putJson(c: Context, keys: OrmKeys[Schema], ar: Array[Any], stream: OutputStream): Unit = {
      var i = 0
      def write(ar: Array[Any], count: Int): Unit = while (i < count) {sendNameValue(this, c, ar, i, stream, keys.list(i)); i += 1 }
      stream.write('{')
      if (keys.links.nonEmpty) {
        JsonToStream.putUnescaped(stream, """"_links":{""")
        write(ar, keys.links.size)
        stream.write('}')
      }
      write(ar, keys.list.size)
      stream.write('}')
    }
  }
  implicit def ormKeysToJsonViaArrayFirstWithDebug[Context, Schema[_]](implicit loggingAdapter: LoggingAdapter, jsonToStreamFor: JsonToStreamFor[Context, Schema]): OrmKeysToJson[Context, Schema] = new OrmKeysToJson[Context, Schema] {
    override def putJson(c: Context, keys: OrmKeys[Schema], ar: Array[Any], stream: OutputStream): Unit = {
      var i = 0
      def write(ar: Array[Any], count: Int): Unit = while (i < count) {
        val ormKey = keys.list(i)
        loggingAdapter.info("OrmKeysToJson")(s"$i ${ormKey.key}  -----  ${ar(i)}")
        sendNameValue(this, c, ar, i, stream, ormKey);
        i += 1
      }
      stream.write('{')
      if (keys.links.nonEmpty) {
        JsonToStream.putUnescaped(stream, """"_links":{""")
        write(ar, keys.links.size)
        stream.write('}')
      }
      write(ar, keys.list.size)
      stream.write('}')
    }
  }

  private def sendNameValue[Context, Schema[_]](ormKeysToJson: OrmKeysToJson[Context, Schema], c: Context, ar: Array[Any], i: Int, stream: OutputStream, ormKey: OrmKey[Schema, _])(implicit jsonToStreamFor: JsonToStreamFor[Context, Schema]): Unit = {
    if (i != 0) stream.write(',')
    JsonToStream.putEscapedWithQuotes(ormKey.key, stream)
    stream.write(':')
    ar(i) match {
      case a: Array[Any] => ormKey.children.writeJsonPrimitive(c, a, stream)(ormKeysToJson)
      case (head: Array[Any]) :: tail =>
        stream.write('[')
        tail.reverse.foreach { case a: Array[Any] =>
          ormKey.children.writeJsonPrimitive(c, a, stream)(ormKeysToJson)
          stream.write(',')
        }
        ormKey.children.writeJsonPrimitive(c, head, stream)(ormKeysToJson)
        stream.write(']')
      case Nil => stream.write('['); stream.write(']')
      case null => JsonToStream.putUnescaped(stream, "null")
      case prim => ormKey.putToJson(stream, c, prim)
    }
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
case class OrmKeys[Schema[_]](links: List[OrmKey[Schema, _]], objects: List[OrmKey[Schema, _]], simple: List[OrmKey[Schema, _]]) {
  val list = links ::: objects ::: simple

  def allKeys: List[OrmKey[Schema, _]] = list.flatMap { key => key :: key.children.allKeys }
  def findForT[T](t: Schema[T]) = allKeys.find(_.t == t)
  def prettyPrint(indent: String) = (links.map(_.prettyPrint(indent, "L")) ::: objects.map(_.prettyPrint(indent, "O")) ::: simple.map(_.prettyPrint(indent, "S"))).mkString("\n")
  def printArray(prefix: String, a: Array[Any], strict: Boolean = true)(implicit asDebugString: AsDebugString[Schema]): List[String] = {
    require(!strict || a.length == size, s"Array size is ${a.length} keys size is $size")
    list.zip(a).flatMap { case (key, item) => key.printArray(prefix, item, strict) }
  }
  def toJson[Context](c: Context, ar: Array[Any])(implicit ormKeyToJson: OrmKeysToJson[Context, Schema]): String = {
    val stream = new ByteArrayOutputStream()
    writeJsonPrimitive(c, ar, stream)
    stream.toString()
  }
  def writeJsonPrimitive[Context](c: Context, ar: Array[Any], outputStream: OutputStream)(implicit ormKeyToJson: OrmKeysToJson[Context, Schema]) =
    ormKeyToJson.putJson(c, this, ar, outputStream)

  val size: Int = list.size
  def makeAndSetupArray: Array[Any] = setup(new Array[Any](size))
  def setup(array: Array[Any]): Array[Any] = {
    var i = 0
    while (i < size) {
      val thisKey = list(i)
      val a = thisKey.arity.optionallyCreateArray(array, i, thisKey.children.size)
      if (a != null) thisKey.children.setup(a)
      i = i + 1
    }
    array
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

  def findLastArrayForList(path: Array[Int], array: Array[Any]): Array[Any] = {
    require(path.length > 0)
    var a = array
    var pathIndex = 0
    while (pathIndex < path.length - 1) {
      val i = path(pathIndex)
      a = asArray(a(i), s"following path ${path.toList.mkString(",")} and ran out Index is $pathIndex index is $i and a is $a")
      pathIndex += 1
    }
    a
  }

  def debugPrint(path: Array[Int], array: Array[Any]): Unit = {
    def printMe(a: Any) = a match {
      case value: Array[_] => value.mkString(",")
      case _ => "" + a
    }
    println("path: " + path.mkString(","))
    var pathIndex = 0
    var a = array
    while (pathIndex < path.length) {
      val i = path(pathIndex)
      //      println(s"  array has length ${a.length} and this Path index is $i")
      val obj = a(i)
      //      println(s"   item at $i is ${printMe(obj)}")
      a = asArray(obj, s"in debug item is $i path is ${path.mkString(",")} obj was $obj")
      pathIndex += 1
    }
  }

  def findKeyForPath[T](path: Array[Int]): OrmKey[Schema, _] = {
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
    val list = asList(lastArray(path(path.length - 1)), s"At end of the path ${path.toList}")
    val ar = findKeyForPath(path).children.makeAndSetupArray
    lastArray(path(path.length - 1)) = ar :: list
  }
}
case class OrmKey[Schema[_], T](arity: ChildArity, key: String, t: Schema[T], path: List[Int], index: Int, children: OrmKeys[Schema]) {
  def putToJson[Context](stream: OutputStream, c: Context, prim: Any)(implicit jsonToStreamFor: JsonToStreamFor[Context, Schema]) =
    jsonToStreamFor.putToJson(c, t).put(c, t, prim, stream)
  def printArray(prefix: String, item: Any, strict: Boolean)(implicit asDebugString: AsDebugString[Schema]): List[String] = {
    val newPrefix = if (prefix.isEmpty) index.toString else prefix + "." + index
    arity.prettyPrintArray(newPrefix, this, item, strict)
  }

  def prettyPrint(indent: String, typeSymbol: String): String = {
    val prefix = s"${indent}$key:$typeSymbol(${path.mkString(",")}),$index $arity"
    if (children.size > 0) prefix + "\n" + children.prettyPrint(indent + " ") else prefix
  }

  def size: Int = children.size
}

trait FieldFilter[F[_]] {
  def apply[T](f: F[T]): Boolean
  def filtered(it: Iterable[F[_]]): List[F[_]] = it.filter(apply(_)).toList
}
trait IsLinkFieldFilter[F[_]] extends FieldFilter[F]

trait LinkBuilder[F[_], Data] {
  def apply[T](f: F[T], lf: Data): String
}


trait IsObjectFieldFilter[F[_]] extends FieldFilter[F]
object IsObjectFieldFilter {
  implicit def isObject[F[_]](implicit hasChildren: HasChildrenForHolder[F]): IsObjectFieldFilter[F] = new IsObjectFieldFilter[F] {
    override def apply[T](f: F[T]): Boolean = hasChildren(f).nonEmpty
  }
}

trait TableNameForManySchema[Schema[_]] {
  def apply[T](s: Schema[T]): Option[TableName]
}

object TableNameForManySchema {
  def apply[S[_]](keysToTableNames: Map[String, TableName])(implicit schemaMapKey: SchemaMapKey[S]): TableNameForManySchema[S] = new TableNameForManySchema[S] {
    override def apply[T](s: S[T]): Option[TableName] = keysToTableNames.get(s.key)
  }
}

trait IsSimpleFieldFilter[F[_]] extends FieldFilter[F]
object IsSimpleFieldFilter {
  implicit def isSimple[F[_]](implicit isLink: IsLinkFieldFilter[F], isObject: IsObjectFieldFilter[F]): IsSimpleFieldFilter[F] = new IsSimpleFieldFilter[F] {
    override def apply[T](f: F[T]): Boolean = !(isLink(f) || isObject(f))
  }
}
object FieldFilter {
  def partition[F[_], Res](ts: List[F[_]], builder: (List[F[_]], List[F[_]], List[F[_]]) => Res)
                          (implicit isSimpleFieldFilter: IsSimpleFieldFilter[F], isLinkFieldFilter: IsLinkFieldFilter[F], isObjectFieldFilter: IsObjectFieldFilter[F]): Res = {
    val links = isLinkFieldFilter.filtered(ts)
    val objects = isObjectFieldFilter.filtered(ts)
    val simple = isSimpleFieldFilter.filtered(ts)
    val all = links ::: objects ::: simple
    require(all.size == all.toSet.size,
      s"""Duplicates. Is your link/object/simple strategy set up so that each item is only in one?\n
         |${all.groupBy(x => x).collect { case (k, list) if list.size > 1 => s"count ${list.size} Item  $k" }.mkString("\n")}
         |""".stripMargin)
    builder(links, objects, simple)
  }
}

object OrmKeys {
  def fromSchema[Schema[_] : SchemaMapKey : IsLinkFieldFilter : IsObjectFieldFilter : IsSimpleFieldFilter](t: Schema[_]): OrmKeys[Schema] = recurse(List(), t.children.children)
  def fromList[Schema[_] : SchemaMapKey : IsLinkFieldFilter : IsObjectFieldFilter : IsSimpleFieldFilter](ts: List[Schema[_]]): OrmKeys[Schema] = recurse(List(), ts)
  protected def recurse[Schema[_] : IsLinkFieldFilter : IsObjectFieldFilter : IsSimpleFieldFilter]
  (parents: List[Int], ts: List[Schema[_]])
  (implicit schemaMapKey: SchemaMapKey[Schema]): OrmKeys[Schema] = {
    def toOrmKeys(ts: List[Schema[_]], offset: Int) =
      ts.zipWithIndex.map { case (t, i) =>
        val index = i + offset
        val children: ChildrenInSchema[Schema] = schemaMapKey.children(t)
        OrmKey(children.arity, schemaMapKey.childKey(t), t, parents, index, recurse(parents :+ index, children.children))
      }
    FieldFilter.partition[Schema, OrmKeys[Schema]](ts, { (links, objects, simple) =>
      OrmKeys(links = toOrmKeys(links, 0), objects = toOrmKeys(objects, links.size), simple = toOrmKeys(simple, objects.size + links.size))
    })
  }
}
