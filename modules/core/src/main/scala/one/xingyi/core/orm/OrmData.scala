package one.xingyi.core.orm

import java.util.Comparator

import one.xingyi.core.orm.OrmData.{arrayToString, childrenToPrintString}

object FlyweightKey {
  def apply(key1: KeysAndIndex, key2: KeysAndIndex): FlyweightKey = recurse(key1.list, key2.list)
  def recurse(list1: List[(Int, FieldType[_])], list2: List[(Int, FieldType[_])]): FlyweightKey = {
    val first = if (list1.head._2.numericSort) KeyInt(list1.head._1, list2.head._1) else KeyString(list1.head._1, list2.head._1)
    if (list1.size == 1) first else Key2(first, recurse(list1.tail, list2.tail))
  }
}

trait FlyweightKey extends Comparator[List[Any]]
case class KeyString(leftIndex: Int, rightIndex: Int) extends FlyweightKey {
  override def compare(o1: List[Any], o2: List[Any]): Int = o1(leftIndex).toString.compareTo(o2(rightIndex).toString)
}
case class KeyInt(leftIndex: Int, rightIndex: Int) extends FlyweightKey {
  override def compare(o1: List[Any], o2: List[Any]): Int =
    o1(leftIndex).asInstanceOf[Int].compareTo(o2(rightIndex).asInstanceOf[Int])
}
case class Key2(key1: FlyweightKey, key2: FlyweightKey) extends FlyweightKey {
  override def compare(o1: List[Any], o2: List[Any]): Int = {
    key1.compare(o1, o2) match {
      case 0 => key2.compare(o1, o2)
      case x => x
    }
  }
}
trait OrmData[Context] {
  def fromParent(context: Context, parent: List[Any]): Context
  def prettyString(indent: String): String
}
object OrmData {
  def arrayToString(indent: String, a: Array[List[Any]]) = a.map(indent + _).mkString("\n")
  def childrenToPrintString[Context](indent: String, children: List[OrmData[Context]]) = if (children.isEmpty) "" else s"\n${indent}children(\n${children.map(_.prettyString(indent)).mkString("\n")})\n"


}

case class MainOrmData[Context, T](t: T, name: String, contextMaker: () => Context, executeWhenMatch: (Context, T, List[Any]) => Context, ar: Array[List[Any]], children: List[OrmData[Context]]) {
  def prettyString = s"MainOrmData($name\n${arrayToString(" ", ar)}\n${childrenToPrintString(" ", children)})\n"
  def applyAll(): Stream[Context] = {
    ar.toStream.map { oneRow =>
      val c1 = executeWhenMatch(contextMaker(), t, oneRow)
      children.foldLeft(c1)((context, child) => child.fromParent(context, oneRow))
    }
  }
}
/** The data read from the database by ORM. Later this should be replaceable by a cursor
 *
 * This is just for the 'numeric keys' story
 *
 * So important features:*
 * <ul><li> We need to process one to many tables and their children sensibly using the next mechanism</li>
 * <li>That means we will process all the things for my id, and then tell the children</li>
 * The data is sorted by id. Children are sorted by child Id
 * Note that we need the id to by ordered...
 * ANd that's awkward because we don't know how big the composible key is...
 * suppose we have Keys1(a), Keys2(a,b), Keys3(a,b,c)... ah do we actually need the id or can we go flyweight?
 * */

case class FanoutOrmData[Context, T](t: T, name: String, flyweightKey: FlyweightKey, executeWhenMatch: (Context, T, List[Any]) => Context, ar: Array[List[Any]], children: List[OrmData[Context]]) extends OrmData[Context] {
  def prettyString(indent: String) = s"${indent}Fanout($name,$flyweightKey\n${arrayToString(indent + " ", ar)}${childrenToPrintString(indent + " ", children)}$indent)"
  var i = 0
  override def fromParent(context: Context, parent: List[Any]): Context = {
    var cont = true
    var thisContext = context
    while (i < ar.length && cont) {
      val thisRow = ar(i)
      val comparison = flyweightKey.compare(parent, thisRow)
      //      println(s"$name $i Parent${parent.mkString(",")} ThisRow${thisRow.mkString(",")} $comparison")
      comparison match {
        case x if x < 0 => cont = false //we are waiting for the parent to catch up
        case 0 =>
          thisContext = executeWhenMatch(context, t, thisRow);
          thisContext = children.foldLeft(thisContext)((context, child) => child.fromParent(context, thisRow))
          i += 1
        case _ => throw new IllegalStateException(s"Not sure how this happened. $t $i\nParent:  $parent\nthisRow: $thisRow")
      }
    }
    thisContext
  }
}

case class FanInOrmData[Context, T](t: T, name: String, idInParentData: GetKey, idForChild: GetKey, executeWhenMatch: (Context, T, List[Any]) => Context, data: Array[List[Any]], children: List[OrmData[Context]]) extends OrmData[Context] {
  def prettyString(indent: String) =
    s"${indent}Fanout($name,idInParent=${idInParentData}, idForChild=${idForChild}\n${arrayToString(indent + " ", data)}\n${childrenToPrintString(indent + " ", children)}\n$indent)"
  val map: Map[Any, List[Any]] = data.foldLeft(Map[Any, List[Any]]())((acc, oneRow) => acc + (idForChild(oneRow) -> oneRow))
  override def fromParent(context: Context, parentRow: List[Any]): Context = {
    map.get(idInParentData(parentRow)).foldLeft(context) { (context, oneRow) =>
      val c1 = (executeWhenMatch(context, t, oneRow))
      children.foldLeft(c1)((context, child) => child.fromParent(context, oneRow))
    }
  }
}