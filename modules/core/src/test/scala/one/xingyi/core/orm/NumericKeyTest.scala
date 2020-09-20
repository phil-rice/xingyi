package one.xingyi.core.orm

import one.xingyi.core.UtilsSpec
import one.xingyi.core.strings.Strings
import org.scalatest.Matchers

import scala.language.implicitConversions

trait NumericKeyFixture extends UtilsSpec {
  object SchemaForTest {
    implicit def debugArray: AsDebugString[SchemaForTest] = (t: SchemaForTest) => "{" + t.key + "}"
  }
  sealed trait SchemaForTest {def key: String}
  case class SchemaItem(key: String) extends SchemaForTest
  case class SchemaItemWithChildren(key: String, hasMany: Boolean, children: List[SchemaForTest]) extends SchemaForTest

  implicit object ObjectKeyMapForTest extends SchemaMapKey[SchemaForTest] {
    override def childKey(t: SchemaForTest): String = t.key
    override def children(t: SchemaForTest): ChildrenInSchema[SchemaForTest] = t match {
      case item: SchemaItem => Zero()
      case SchemaItemWithChildren(_, true, children) => ZeroOrMore(children)
      case SchemaItemWithChildren(_, false, children) => AlwaysOne(children)
    }
  }

  val itema: SchemaForTest = SchemaItem("a")
  val itemb: SchemaForTest = SchemaItem("b")
  val itemc: SchemaForTest = SchemaItem("c")
  val emptyNumbericKeys: NumericKeys[SchemaForTest] = NumericKeys(List())
  val emptySkeleton: NumericKeys[SchemaForTest] = emptyNumbericKeys
  val bcAsSingleton: SchemaForTest = SchemaItemWithChildren("bc", false, List(itemb, itemc))
  val bcAsMany: SchemaForTest = SchemaItemWithChildren("bc", true, List(itemb, itemc))

  def numericKeysForBc(path: List[Int]): NumericKeys[SchemaForTest] = NumericKeys(List(
    NumericKey(path, 0, "b", NoChildren, emptySkeleton, itemb),
    NumericKey(path, 1, "c", NoChildren, emptySkeleton, itemc)))

  implicit def stringToSchemaForTest(s: String): SchemaForTest = SchemaItem(s)

  val ab3m: SchemaItemWithChildren = SchemaItemWithChildren("m", false, List("10"))
  val ab3: SchemaItemWithChildren = SchemaItemWithChildren("3", true, List[SchemaForTest]("n", ab3m))
  val ab: SchemaItemWithChildren = SchemaItemWithChildren("b", false, List[SchemaForTest]("1", "2", ab3))
  val complex = List[SchemaForTest]("a", ab)


  def checkNumericKeys[T](n: NumericKeys[T])(expected: String) =
    checkStrings(n.prettyPrint(""), expected)

  def checkArray[T: AsDebugString](key: NumericKeys[T], a: Array[Any])(expected: String) =
    checkStrings(key.printArray("", a).mkString("\n"), expected)

}
class NumericKeyTest extends  NumericKeyFixture {


  behavior of "NumericKeys with Map[String,Any]"

  it should "turn the empty list into a blank skeleton" in {
    NumericKeys.apply(List()) shouldBe emptySkeleton
  }

  it should "turn a one item list into a simple skeleton" in {
    NumericKeys(List(itema)) shouldBe NumericKeys(List(NumericKey(List(), 0, "a", NoChildren, emptySkeleton, itema)))
  }
  it should "turn a two item list into a simple skeleton" in {
    NumericKeys(List(itemb, itemc)) shouldBe numericKeysForBc(List())
  }
  it should "turn a two item list with children into a simple skeleton" in {
    NumericKeys(List(itema, bcAsSingleton)) shouldBe NumericKeys(List(
      NumericKey(List(), 0, "a", NoChildren, emptySkeleton, itema),
      NumericKey(List(), 1, "bc", OneChild, numericKeysForBc(List(1)), bcAsSingleton)))
    checkNumericKeys(NumericKeys(List(itema, bcAsSingleton))) {
      """a (),0 NoChildren
        |bc (),1 OneChild
        | b (1),0 NoChildren
        | c (1),1 NoChildren""".stripMargin
    }

    NumericKeys(List(bcAsMany, itema)) shouldBe NumericKeys(List(
      NumericKey(List(), 0, "bc", ManyChildren, numericKeysForBc(List(0)), bcAsMany),
      NumericKey(List(), 1, "a", NoChildren, emptySkeleton, itema)))

  }

  it should "turn a complicated schema into a skeleton" in {
    checkNumericKeys(NumericKeys(complex))(
      """a (),0 NoChildren
        |b (),1 OneChild
        | 1 (1),0 NoChildren
        | 2 (1),1 NoChildren
        | 3 (1),2 ManyChildren
        |  n (1,2),0 NoChildren
        |  m (1,2),1 OneChild
        |   10 (1,2,1),0 NoChildren""".stripMargin)
  }

  behavior of "makeArray"

  it should "make an array for simple things" in {
    emptyNumbericKeys.makeAndSetupArray.toList shouldBe List()
    val bc = NumericKeys(List(itemb, itemc))
    val a = bc.makeAndSetupArray
    checkArray(bc, a)(
      """0  = null {b}
        |1  = null {c}""".stripMargin)
  }
  it should "make an array for things with singleton children" in {
    val keys = NumericKeys(List(itema, bcAsSingleton))
    val array = keys.makeAndSetupArray
    checkArray(keys, array)(
      """0  = null {a}
        |1/OneChild
        |1.0  = null {b}
        |1.1  = null {c}""".stripMargin)
  }
  it should "make an array for things with many children" in {
    val keys = NumericKeys(List(itema, bcAsMany))
    val array = keys.makeAndSetupArray
    checkArray(keys, array)(
      """0  = null {a}
        |1/Many(1)
        |1[0].0  = null {b}
        |1[0].1  = null {c}""".stripMargin)
  }

  behavior of "NumericKeys/put"

  it should "put an item into a simple array" in {
    val keys = NumericKeys(List(itema, itemb))
    val array = keys.makeAndSetupArray
    keys.put(Array(), 0, array, "someData")
    checkArray(keys, array)(
      """0  = someData {a}
        |1  = null {b}""".stripMargin)
    keys.put(Array(), 1, array, "someMoreData")
    checkArray(keys, array)(
      """0  = someData {a}
        |1  = someMoreData {b}""".stripMargin)

  }
  it should "put an item into an array with a singleton child" in {
    val keys = NumericKeys(List(itema, bcAsSingleton))
    val array = keys.makeAndSetupArray
    keys.put(Array(1), 0, array, "someData")
    checkArray(keys, array)(
      """0  = null {a}
        |1/OneChild
        |1.0  = someData {b}
        |1.1  = null {c}""".stripMargin)

  }
  it should "put an item into an array with a many child" in {
    val keys = NumericKeys(List(itema, bcAsMany))
    val array = keys.makeAndSetupArray
    keys.put(Array(1), 0, array, "someData")
    checkArray(keys, array)(
      """0  = null {a}
        |1/Many(1)
        |1[0].0  = someData {b}
        |1[0].1  = null {c}""".stripMargin)

  }

  it should "put an item into a complex array" in {
    val keys = NumericKeys(complex)
    val array = keys.makeAndSetupArray
    keys.put(Array(1), 0, array, "someData")
    checkArray(keys, array)(
      """0  = null {a}
        |1/OneChild
        |1.0  = someData {1}
        |1.1  = null {2}
        |1.2/Many(1)
        |1.2[0].0  = null {n}
        |1.2[0].1/OneChild
        |1.2[0].1.0  = null {10}""".stripMargin)
    keys.put(Array(1, 2), 0, array, "someMoreData")
    checkArray(keys, array)(
      """0  = null {a}
        |1/OneChild
        |1.0  = someData {1}
        |1.1  = null {2}
        |1.2/Many(1)
        |1.2[0].0  = someMoreData {n}
        |1.2[0].1/OneChild
        |1.2[0].1.0  = null {10}""".stripMargin)
    keys.put(Array(1, 2, 1), 0, array, "atEndOfChain")
    checkArray(keys, array)(
      """0  = null {a}
        |1/OneChild
        |1.0  = someData {1}
        |1.1  = null {2}
        |1.2/Many(1)
        |1.2[0].0  = someMoreData {n}
        |1.2[0].1/OneChild
        |1.2[0].1.0  = atEndOfChain {10}""".stripMargin)
  }

  it should "not allow replacing of an array or a list" in {
    def checkException(block: => Unit): Unit = {
      try {
        block
        fail
      } catch {case e: IllegalStateException =>}
    }
    val keys = NumericKeys(complex)
    val array = keys.makeAndSetupArray
    checkException(keys.put(Array(), 1, array, "someData"))
    checkException(keys.put(Array(1), 2, array, "someData"))
  }

  behavior of "findKey"

  it should "find the key for the index" in {
    val keys = NumericKeys(complex)
    def check(path: Array[Int], expectedPath: List[Int], expectedIndex: Int, expectedKey: String) {
      val key = keys.findKeyForPath(path)
      key.path shouldBe expectedPath
      key.index shouldBe expectedIndex
      key.key shouldBe expectedKey
    }
    check(Array(0), List(), 0, "a")
    check(Array(1), List(), 1, "b")
    check(Array(1, 0), List(1), 0, "1")
    check(Array(1, 1), List(1), 1, "2")
    check(Array(1, 2), List(1), 2, "3")
    check(Array(1, 2, 0), List(1, 2), 0, "n")
  }

  behavior of "next"

  it should "add new arrays" in {
    val keys = NumericKeys(complex)
    val array = keys.makeAndSetupArray
    keys.next(Array(1, 2), array)
    checkArray(keys, array)(
      """0  = null {a}
        |1/OneChild
        |1.0  = null {1}
        |1.1  = null {2}
        |1.2/Many(2)
        |1.2[0].0  = null {n}
        |1.2[0].1/OneChild
        |1.2[0].1.0  = null {10}
        |1.2[1].0  = null {n}
        |1.2[1].1/OneChild
        |1.2[1].1.0  = null {10}""".stripMargin)
    keys.put(Array(1, 2, 1), 0, array, "ShouldChangeHeadOfList")
    checkArray(keys, array)(
      """0  = null {a}
        |1/OneChild
        |1.0  = null {1}
        |1.1  = null {2}
        |1.2/Many(2)
        |1.2[0].0  = null {n}
        |1.2[0].1/OneChild
        |1.2[0].1.0  = ShouldChangeHeadOfList {10}
        |1.2[1].0  = null {n}
        |1.2[1].1/OneChild
        |1.2[1].1.0  = null {10}""".stripMargin)

  }
  it should "not allow next to be called if the path isn't a many child" in {
    val keys = NumericKeys(complex)
    val array = keys.makeAndSetupArray
    def check(path: Int*) = try {
      keys.next(path.toArray, array)
      fail()
    } catch {case e: IllegalArgumentException =>}
    check(1)
    check(1, 0)
    check(1, 2, 1)
  }

}
