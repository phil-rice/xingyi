package one.xingyi.core.orm

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
    s"$prefix  = $item ${asDebugString(key.t)}"
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
    val result = new Array[Any](size)
    a(i) = List(result)
    result
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

//THere is a schema of type T. T is not the data, is the structure of the data
trait SchemaMapKey[T] {
  def childKey(t: T): String // The main object might not have a key, but the children will
  def children(t: T): ChildrenInSchema[T]
}

case class NumericKeys[T](list: List[NumericKey[T]]) {
  def allKeys: List[NumericKey[T]] = list.flatMap { key => key :: key.children.allKeys }
  def prettyPrint(indent: String) = list.map(_.prettyPrint(indent)).mkString("\n")
  def printArray(prefix: String, a: Array[Any], strict: Boolean = true)(implicit asDebugString: AsDebugString[T]): List[String] = {
    require(!strict || a.length == size, s"Array size is ${a.length} keys size is $size")
    list.zip(a).flatMap { case (key, item) => key.printArray(prefix, item, strict) }

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
      thisKey.children.setup(a)
      i = i + 1
    }
  }

  def asArray(a: Any, msg: => String): Array[Any] = a match {
    case ar: Array[Any] => ar
    case list: List[_] => list.head.asInstanceOf[Array[Any]]
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
  def findKeyForPath(path: Array[Int]): NumericKey[T] = {
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
case class NumericKey[T](path: List[Int], index: Int, key: String, arity: ChildArity, children: NumericKeys[T], t: T) {
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

object NumericKeys {
  def apply[T: SchemaMapKey](ts: List[T]): NumericKeys[T] = recurse(List(), ts)
  def recurse[T](parents: List[Int], ts: List[T])(implicit schemaMapKey: SchemaMapKey[T]): NumericKeys[T] =
    NumericKeys(ts.zipWithIndex.map { case (t, index) =>
      val children: ChildrenInSchema[T] = schemaMapKey.children(t)
      NumericKey(parents, index, schemaMapKey.childKey(t), children.arity, recurse(parents :+ index, children.children), t)
    })
}


