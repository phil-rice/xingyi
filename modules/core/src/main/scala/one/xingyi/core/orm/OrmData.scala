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
trait OrmData {
  def fromParent(parent: List[Any])
  def prettyString(indent: String): String
}
object OrmData {
  def arrayToString(indent: String, a: Array[List[Any]]) = a.map(indent + _).mkString("\n")
  def childrenToPrintString(indent: String, children: List[OrmData]) = if (children.isEmpty) "" else s"\n${indent}children(\n${children.map(_.prettyString(indent)).mkString("\n")})\n"
}

case class MainOrmData[T](t: T, name: String, executeWhenMatch: (T, List[Any]) => Unit, ar: Array[List[Any]], children: List[OrmData]) {
  def prettyString = s"MainOrmData($name\n${arrayToString(" ", ar)}\n${childrenToPrintString(" ", children)})\n"
  def applyAll(): Unit = {
    ar.foreach { oneRow =>
      executeWhenMatch(t, oneRow)
      children.foreach(_.fromParent(oneRow))
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

case class FanoutOrmData[T](t: T, name: String, flyweightKey: FlyweightKey, executeWhenMatch: (T, List[Any]) => Unit, ar: Array[List[Any]], children: List[OrmData]) extends OrmData {
  def prettyString(indent: String) = s"${indent}Fanout($name,$flyweightKey\n${arrayToString(indent + " ", ar)}${childrenToPrintString(indent + " ", children)}$indent)"
  var i = 0
  def fromParent(parent: List[Any]) {
    var cont = true
    while (i < ar.length && cont) {
      val thisRow = ar(i)
      val comparison = flyweightKey.compare(parent, thisRow)
      //      println(s"$name $i Parent${parent.mkString(",")} ThisRow${thisRow.mkString(",")} $comparison")
      comparison match {
        case x if x < 0 => cont = false //we are waiting for the parent to catch up
        case 0 =>
          executeWhenMatch(t, thisRow);
          children.foreach(_.fromParent(thisRow))
          i += 1
        case _ => throw new IllegalStateException(s"Not sure how this happened. $t $i\nParent:  $parent\nthisRow: $thisRow")
      }
    }
  }
}

case class FanInOrmData[T](t: T, name: String, idInParentData: GetKey, idForChild: GetKey, executeWhenMatch: (T, List[Any]) => Unit, data: Array[List[Any]], children: List[OrmData]) extends OrmData {
  def prettyString(indent: String) =
    s"${indent}Fanout($name,idInParent=${idInParentData}, idForChild=${idForChild}\n${arrayToString(indent + " ", data)}\n${childrenToPrintString(indent + " ", children)}\n$indent)"
  val map: Map[Any, List[Any]] = data.foldLeft(Map[Any, List[Any]]())((acc, oneRow) => acc + (idForChild(oneRow) -> oneRow))
  override def fromParent(parentRow: List[Any]): Unit = {
    map.get(idInParentData(parentRow)) foreach { oneRow =>
      (executeWhenMatch(t, oneRow))
      children.foreach(_.fromParent(oneRow))
    }
  }
}