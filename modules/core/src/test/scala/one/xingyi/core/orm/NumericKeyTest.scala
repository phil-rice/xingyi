package one.xingyi.core.orm

import java.io.ByteArrayOutputStream

import one.xingyi.core.UtilsSpec
import one.xingyi.core.strings.Strings
import org.scalatest.Matchers

import scala.language.implicitConversions


trait NumericKeyFixture extends UtilsSpec {
  implicit def multipleFieldTx[T]: OrmValueTransformer[T] = (ftis, data) => ftis.map(fti => fti.fieldType.name + ":" + data(fti.index)).mkString(",")

  sealed trait SchemaForTest {def key: String}
  case class SchemaItem(key: String) extends SchemaForTest
  case class SchemaItemWithChildren(key: String, hasMany: Boolean, children: List[SchemaForTest]) extends SchemaForTest

  object SchemaForTest {
    def parse[T: OrmValueTransformer](s: String): List[OrmValueGetter[_]] = {
      val mainSplitter = Strings.split(";")
      val fieldsSplitter = Strings.split(",")
      mainSplitter(s).flatMap { tf =>
        if (tf.contains("/")) {
          val (name, fields) = Strings.splitInTwo("/")(tf)
          List(OrmValueGetter(TableName(name, ""), fieldsSplitter(fields).map(FieldType.apply)))
        }
        else List()
      }
    }
    implicit val findKeys: FindOrmEntityAndField[SchemaForTest] = { item: SchemaForTest => SchemaForTest.parse(item.key) }

    implicit def debugArray: AsDebugString[SchemaForTest] = (t: SchemaForTest) => "{" + t.key + "}"
    implicit object ObjectKeyMapForTest extends SchemaMapKey[SchemaForTest] {
      override def childKey(t: SchemaForTest): String = t.key
      override def children(t: SchemaForTest): ChildrenInSchema[SchemaForTest] = t match {
        case item: SchemaItem => Zero()
        case SchemaItemWithChildren(_, true, children) => ZeroOrMore(children)
        case SchemaItemWithChildren(_, false, children) => AlwaysOne(children)
      }
    }
  }
  def checkNumericKeys[T](n: NumericKeys[T])(expected: String) =
    checkStrings(n.prettyPrint(""), expected)

  def checkArray[T: AsDebugString](key: NumericKeys[T], a: Array[Any])(expected: String) = {
    require(key != null)
    require(a != null)
    checkStrings(key.printArray("", a).mkString("\n"), expected)
  }

}
trait NumericKeySpecFixture extends NumericKeyFixture {
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


}
class NumericKeyTest extends NumericKeySpecFixture {

  behavior of "SchemaItemParser - checking test DSL"
  val t1 = TableName("t1", "")
  val t2 = TableName("t2", "")
  it should "turn a string into tables/field list" in {
    type NotImportant = Any
    SchemaForTest.parse("") shouldBe List()
    SchemaForTest.parse("justAstring") shouldBe List()
    SchemaForTest.parse("one;two;three") shouldBe List()
    SchemaForTest.parse("t1/f1:int") shouldBe List(OrmValueGetter[NotImportant](t1, List(FieldType("f1:int"))))
    SchemaForTest.parse("t1/f1:int,f2") shouldBe List(OrmValueGetter[NotImportant](t1, List(FieldType("f1:int"), FieldType("f2"))))
    SchemaForTest.parse("t1/f1:int,f2,f3") shouldBe List(OrmValueGetter[NotImportant](t1, List(FieldType("f1:int"), FieldType("f2"), FieldType("f3"))))
    SchemaForTest.parse("t1/f1:int,f2;t2/f3,f4:int") shouldBe List(
      OrmValueGetter[NotImportant](t1, List(FieldType("f1:int"), FieldType("f2"))),
      OrmValueGetter[NotImportant](t2, List(FieldType("f3"), FieldType("f4:int"))))
  }


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
  it should "make an list for things with many children" in {
    val keys = NumericKeys(List(itema, bcAsMany))
    val array = keys.makeAndSetupArray
    checkArray(keys, array)(
      """0  = null {a}
        |1/Many(0)""".stripMargin)
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
    keys.next(Array(1), array)
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
        |1.2/Many(0)""".stripMargin)
    keys.next(Array(1, 2), array)
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
        |1.2/Many(1)
        |1.2[0].0  = null {n}
        |1.2[0].1/OneChild
        |1.2[0].1.0  = null {10}""".stripMargin)
    keys.put(Array(1, 2, 1), 0, array, "ShouldChangeHeadOfList")
    checkArray(keys, array)(
      """0  = null {a}
        |1/OneChild
        |1.0  = null {1}
        |1.1  = null {2}
        |1.2/Many(1)
        |1.2[0].0  = null {n}
        |1.2[0].1/OneChild
        |1.2[0].1.0  = ShouldChangeHeadOfList {10}""".stripMargin)
    keys.next(Array(1, 2), array)
    keys.put(Array(1, 2, 1), 0, array, "AndAgain")
    checkArray(keys, array)(
      """0  = null {a}
        |1/OneChild
        |1.0  = null {1}
        |1.1  = null {2}
        |1.2/Many(2)
        |1.2[0].0  = null {n}
        |1.2[0].1/OneChild
        |1.2[0].1.0  = AndAgain {10}
        |1.2[1].0  = null {n}
        |1.2[1].1/OneChild
        |1.2[1].1.0  = ShouldChangeHeadOfList {10}""".stripMargin)

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
    keys.next(Array(1, 2), array)
    check(1, 2, 1)
  }


  behavior of "NumericKeys/Json"

  def checkJson[T](keys: NumericKeys[T], array: Array[Any], expected: String): Unit = {
    val stream = new ByteArrayOutputStream()
    keys.putJson(array, stream)
    checkStrings(stream.toString(), expected)
  }
  it should "turn an array into json" in {
    val keys = NumericKeys(List(itema, itemb, itemc))
    val array = keys.makeAndSetupArray
    keys.put(Array(), 0, array, "someData")
    keys.put(Array(), 1, array, 123)
    keys.put(Array(), 2, array, 1.2)
    checkArray(keys, array)(
      """0  = someData {a}
        |1  = 123 {b}
        |2  = 1.2 {c}""".stripMargin)
    checkJson(keys, array, """{"a":"someData","b":123,"c":1.2}""")
  }

  it should "handle nulls" in {
    val keys = NumericKeys(List(itema, itemb, itemc))
    val array = keys.makeAndSetupArray
    checkArray(keys, array)(
      """0  = null {a}
        |1  = null {b}
        |2  = null {c}""".stripMargin)
    checkJson(keys, array, """{"a":null,"b":null,"c":null}""")
  }

  it should "escape characters" in {
    val keys = NumericKeys(List(itema))
    val array = keys.makeAndSetupArray
    keys.put(Array(), 0, array, "so\\me \b \f \n  \r  \t  va\"lue")
    checkArray(keys, array)("""0  = so\\me \b \f \n  \r  \t  va\"lue {a}""".stripMargin)
    checkJson(keys, array, """{"a":"so\\me \b \f \n  \r  \t  va\"lue"}""")
    //    escaped = escaped.replace("\\", "\\\\")
    //    escaped = escaped.replace("\"", "\\\"")
    //    escaped = escaped.replace("\b", "\\b")
    //    escaped = escaped.replace("\f", "\\f")
    //    escaped = escaped.replace("\n", "\\n")
    //    escaped = escaped.replace("\r", "\\r")
    //    escaped = escaped.replace("\t", "\\t")
  }


  it should "turn an array with child objects into json" in {
    val keys = NumericKeys(List(itema, bcAsSingleton))
    val array = keys.makeAndSetupArray
    keys.put(Array(), 0, array, "av")
    keys.put(Array(1), 0, array, "bv")
    keys.put(Array(1), 1, array, "cv")
    checkArray(keys, array)(
      """0  = av {a}
        |1/OneChild
        |1.0  = bv {b}
        |1.1  = cv {c}""".stripMargin)
    checkJson(keys, array, """{"a":"av","bc":{"b":"bv","c":"cv"}}""")
  }

  it should "turn an array with child arrays into json" in {
    val keys = NumericKeys(List(itema, bcAsMany))
    val array = keys.makeAndSetupArray
    keys.put(Array(), 0, array, "av")
    checkArray(keys, array)(
      """0  = av {a}
        |1/Many(0)""".stripMargin)
    checkJson(keys, array, """{"a":"av","bc":[]}""")

    keys.next(Array(1), array)
    keys.put(Array(1), 0, array, "bv1")
    keys.put(Array(1), 1, array, "cv1")
    checkJson(keys, array, """{"a":"av","bc":[{"b":"bv1","c":"cv1"}]}""")

    keys.next(Array(1), array)
    keys.put(Array(1), 0, array, "bv2")
    keys.put(Array(1), 1, array, "cv2")

    checkArray(keys, array)(
      """0  = av {a}
        |1/Many(2)
        |1[0].0  = bv2 {b}
        |1[0].1  = cv2 {c}
        |1[1].0  = bv1 {b}
        |1[1].1  = cv1 {c}""".stripMargin)
    checkJson(keys, array, """{"a":"av","bc":[{"b":"bv1","c":"cv1"},{"b":"bv2","c":"cv2"}]}""")
  }
}
