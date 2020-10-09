package one.xingyi.core.orm

import java.io.{ByteArrayOutputStream, OutputStream}

import one.xingyi.core.UtilsSpec
import one.xingyi.core.aggregate.{HasChildren, HasChildrenForHolder}
import one.xingyi.core.strings.Strings

import scala.language.{higherKinds, implicitConversions}


trait OrmKeyFixture extends UtilsSpec {
  implicit def multipleFieldTx: OrmValueTransformer[String] = (ftis, data) => ftis.map(fti => fti.fieldType.name + ":" + data(fti.index)).mkString(",")

  sealed trait SchemaForTest[T] {
    def key: String
    def jsonToStream: JsonToStream[String, SchemaForTest, T]
  }
  case class SchemaItem[T](key: String)(implicit val jsonToStream: JsonToStream[String, SchemaForTest, T]) extends SchemaForTest[T]
  case class SchemaItemWithChildren(key: String, hasMany: Boolean, children: List[SchemaForTest[_]])
                                   (implicit val jsonToStream: JsonToStream[String, SchemaForTest, Placeholder]) extends SchemaForTest[Placeholder]

  object SchemaForTest {
    def parse[T: OrmValueTransformer](s: String): List[OrmValueGetter[_]] = {
      val mainSplitter = Strings.split(";")
      val fieldsSplitter = Strings.split(",")
      mainSplitter(s).flatMap { tf =>
        if (tf.contains("/")) {
          val (name, fields) = Strings.splitInTwo("/")(tf)
          List(OrmValueGetter[String](TableName(name, ""), fieldsSplitter(fields).map(FieldType.apply)))
        }
        else List()
      }
    }
    implicit val findKeys: FindOrmEntityAndField[SchemaForTest] = new FindOrmEntityAndField[SchemaForTest] {
      override def apply[T](s: SchemaForTest[T]): List[OrmValueGetter[_]] = SchemaForTest.parse(s.key)
    }
    implicit def JsonToStreamFor: JsonToStreamFor[String, SchemaForTest] = new JsonToStreamFor[String, SchemaForTest] {
      override def putToJson[T](context: String, s: SchemaForTest[T]): JsonToStream[String, SchemaForTest, T] = s.jsonToStream
    }


    implicit val isLink: IsLinkFieldFilter[SchemaForTest] = new IsLinkFieldFilter[SchemaForTest] {
      def apply[T](f: SchemaForTest[T]): Boolean = hasChildren(f).isEmpty && (f.key endsWith "c")
    }
    implicit def hasChildren: HasChildrenForHolder[SchemaForTest] = new HasChildrenForHolder[SchemaForTest] {
      override def apply[T](f: SchemaForTest[T]): List[SchemaForTest[_]] = f match {
        case s: SchemaItemWithChildren => s.children
        case _ => Nil
      }
    }

    implicit def debugArray: AsDebugString[SchemaForTest] = new AsDebugString[SchemaForTest] {
      override def apply[T](t: SchemaForTest[T]): String = "{" + t.key + "}"
      override def data(t: Any): String = t.toString
    }
    implicit object ObjectKeyMapForTest extends SchemaMapKey[SchemaForTest] {
      override def childKey[T](t: SchemaForTest[T]): String = t.key
      override def children[T](t: SchemaForTest[T]): ChildrenInSchema[SchemaForTest] = t match {
        case item: SchemaItem[_] => Zero()
        case SchemaItemWithChildren(_, true, children) => ZeroOrMore(children)
        case SchemaItemWithChildren(_, false, children) => AlwaysOne(children)
      }
    }
  }
  def checkKeys[Schema[_]](n: OrmKeys[Schema])(expected: String) =
    checkStrings(n.prettyPrint(""), expected)

  def checkArray[Schema[_] : AsDebugString](key: OrmKeys[Schema], a: Array[Any])(expected: String) = {
    require(key != null)
    require(a != null)
    checkStrings(key.printArray("", a).mkString("\n"), expected)
  }
}

trait OrmKeySpecFixture extends OrmKeyFixture {
  val itema: SchemaForTest[String] = SchemaItem("a")
  val itemb: SchemaForTest[Int] = SchemaItem("b")
  val itemc: SchemaForTest[Double] = SchemaItem("c")
  val emptyNumbericKeys: OrmKeys[SchemaForTest] = OrmKeys.fromList(List[SchemaForTest[_]]())
  val emptySkeleton: OrmKeys[SchemaForTest] = emptyNumbericKeys
  val bcAsSingleton: SchemaForTest[Placeholder] = SchemaItemWithChildren("bc", false, List(itemb, itemc))
  val bcAsMany: SchemaForTest[Placeholder] = SchemaItemWithChildren("bc", true, List(itemb, itemc))

  def numericKeysForBc(path: List[Int]): OrmKeys[SchemaForTest] = OrmKeys(
    links = List(OrmKey(NoChildren, "c", itemc, path, 0, emptySkeleton)),
    objects = List(),
    simple = List(OrmKey(NoChildren, "b", itemb, path, 1, emptySkeleton)))

  implicit def stringToSchemaForTest(s: String): SchemaForTest[String] = SchemaItem(s)

  val ab3m: SchemaItemWithChildren = SchemaItemWithChildren("m", false, List("10"))
  val ab3: SchemaItemWithChildren = SchemaItemWithChildren("3", true, List[SchemaForTest[_]]("n", ab3m))
  val ab: SchemaItemWithChildren = SchemaItemWithChildren("b", false, List[SchemaForTest[_]]("1", "2", ab3))
  val complex = List[SchemaForTest[_]]("a", ab)


}
class OrmKeyTest extends OrmKeySpecFixture {

  behavior of "SchemaItemParser - checking test DSL"
  val t1 = TableName("t1", "")
  val t2 = TableName("t2", "")
  it should "turn a string into tables/field list" in {
    SchemaForTest.parse("") shouldBe List()
    SchemaForTest.parse("justAstring") shouldBe List()
    SchemaForTest.parse("one;two;three") shouldBe List()
    SchemaForTest.parse("t1/f1:int") shouldBe List(OrmValueGetter[String](t1, List(FieldType("f1:int"))))
    SchemaForTest.parse("t1/f1:int,f2") shouldBe List(OrmValueGetter[String](t1, List(FieldType("f1:int"), FieldType("f2"))))
    SchemaForTest.parse("t1/f1:int,f2,f3") shouldBe List(OrmValueGetter[String](t1, List(FieldType("f1:int"), FieldType("f2"), FieldType("f3"))))
    SchemaForTest.parse("t1/f1:int,f2;t2/f3,f4:int") shouldBe List(
      OrmValueGetter[String](t1, List(FieldType("f1:int"), FieldType("f2"))),
      OrmValueGetter[String](t2, List(FieldType("f3"), FieldType("f4:int"))))
  }


  behavior of "checking fieldFilters"

  it should "give an issue if an item is duplicated" in {
    implicit val isLinkFieldFilter: IsLinkFieldFilter[SchemaForTest] = new IsLinkFieldFilter[SchemaForTest] {
      override def apply[T](f: SchemaForTest[T]): Boolean = true
    }
    implicit val isObjectFieldFilter: IsObjectFieldFilter[SchemaForTest] = new IsObjectFieldFilter[SchemaForTest] {
      override def apply[T](f: SchemaForTest[T]): Boolean = true
    }
    try {
      val keys: OrmKeys[SchemaForTest] = OrmKeys.fromList(List(itema, itemb))
      fail()
    } catch {
      case e: IllegalArgumentException =>
        checkStrings(e.getMessage,
          """requirement failed: Duplicates. Is your link/object/simple strategy set up so that each item is only in one?
            |
            |count 2 Item  SchemaItem(b)
            |count 2 Item  SchemaItem(a)""".stripMargin)
    }
  }

  behavior of "NumericKeys with Map[String,Any]"

  it should "turn the empty list into a blank skeleton" in {
    OrmKeys.fromList(List[SchemaForTest[_]]()) shouldBe emptySkeleton
  }

  it should "turn a one item list into a simple skeleton" in {
    OrmKeys.fromList(List(itema)) shouldBe OrmKeys(
      objects = List(),
      links = List(),
      simple = List(OrmKey(NoChildren, "a", itema, List(), 0, emptySkeleton)))
  }
  it should "turn a two item list into a simple skeleton" in {
    OrmKeys.fromList(List(itemb, itemc)) shouldBe numericKeysForBc(List())
  }
  it should "turn a two item list with children into a simple skeleton" in {
    OrmKeys.fromList(List(itema, bcAsSingleton)) shouldBe OrmKeys(
      links = List(),
      objects = List(OrmKey(OneChild, "bc", bcAsSingleton, List(), 0, numericKeysForBc(List(0)))),
      simple = List(OrmKey(NoChildren, "a", itema, List(), 1, emptySkeleton)))

    checkKeys(OrmKeys.fromList(List(itema, bcAsSingleton))) {
      """bc:O(),0 OneChild
        | c:L(0),0 NoChildren
        | b:S(0),1 NoChildren
        |a:S(),1 NoChildren""".stripMargin
    }

    OrmKeys.fromList(List(bcAsMany, itema)) shouldBe OrmKeys(
      links = List(),
      objects = List(OrmKey(ManyChildren, "bc", bcAsMany, List(), 0, numericKeysForBc(List(0)))),
      simple = List(OrmKey(NoChildren, "a", itema, List(), 1, emptySkeleton)))

  }

  it should "turn a complicated schema into a skeleton" in {
    checkKeys(OrmKeys.fromList(complex))(
      """b:O(),0 OneChild
        | 3:O(0),0 ManyChildren
        |  m:O(0,0),0 OneChild
        |   10:S(0,0,0),0 NoChildren
        |  n:S(0,0),1 NoChildren
        | 1:S(0),1 NoChildren
        | 2:S(0),2 NoChildren
        |a:S(),1 NoChildren""".stripMargin)
  }

  behavior of "makeArray"

  it should "make an array for simple things" in {
    emptyNumbericKeys.makeAndSetupArray.toList shouldBe List()
    val bc = OrmKeys.fromList(List(itemb, itemc))
    val a = bc.makeAndSetupArray
    checkArray(bc, a)(
      """0  = null {c}
        |1  = null {b}""".stripMargin)
  }
  it should "make an array for things with singleton children" in {
    val keys = OrmKeys.fromList(List(itema, bcAsSingleton))
    val array = keys.makeAndSetupArray
    checkArray(keys, array)(
      """0/OneChild
        |0.0  = null {c}
        |0.1  = null {b}
        |1  = null {a}""".stripMargin)
  }
  it should "make an list for things with many children" in {
    val keys = OrmKeys.fromList(List(itema, bcAsMany))
    val array = keys.makeAndSetupArray
    checkArray(keys, array)(
      """0/Many(0)
        |1  = null {a}""".stripMargin)
  }

  behavior of "NumericKeys/put"

  it should "put an item into a simple array" in {
    val keys = OrmKeys.fromList(List(itema, itemb))
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
    val keys = OrmKeys.fromList(List(itema, bcAsSingleton))
    val array = keys.makeAndSetupArray
    keys.put(Array(0), 0, array, "someData")
    checkArray(keys, array)(
      """0/OneChild
        |0.0  = someData {c}
        |0.1  = null {b}
        |1  = null {a}
        |""".stripMargin)

  }
  it should "put an item into an array with a many child" in {
    val keys = OrmKeys.fromList(List(itema, bcAsMany))
    checkKeys(keys)(
      """bc:O(),0 ManyChildren
        | c:L(0),0 NoChildren
        | b:S(0),1 NoChildren
        |a:S(),1 NoChildren""".stripMargin)
    val array = keys.makeAndSetupArray
    keys.next(Array(0), array)
    keys.put(Array(0), 0, array, "someData")
    checkArray(keys, array)(
      """0/Many(1)
        |0[0].0  = someData {c}
        |0[0].1  = null {b}
        |1  = null {a}""".stripMargin)

  }

  it should "put an item into a complex array" in {
    val keys = OrmKeys.fromList(complex)
    val array = keys.makeAndSetupArray
    keys.put(Array(0), 1, array, "leaf1")
    keys.put(Array(0), 2, array, "leaf2")
    checkArray(keys, array)(
      """0/OneChild
        |0.0/Many(0)
        |0.1  = leaf1 {1}
        |0.2  = leaf2 {2}
        |1  = null {a}""".stripMargin)
    keys.next(Array(0, 0), array)
    checkArray(keys, array)(
      """0/OneChild
        |0.0/Many(1)
        |0.0[0].0/OneChild
        |0.0[0].0.0  = null {10}
        |0.0[0].1  = null {n}
        |0.1  = leaf1 {1}
        |0.2  = leaf2 {2}
        |1  = null {a}""".stripMargin)
    keys.put(Array(0, 0), 1, array, "dataInList0Leaf")
    keys.put(Array(0, 0, 0), 0, array, "dataInList0Child")
    checkArray(keys, array)(
      """0/OneChild
        |0.0/Many(1)
        |0.0[0].0/OneChild
        |0.0[0].0.0  = dataInList0Child {10}
        |0.0[0].1  = dataInList0Leaf {n}
        |0.1  = leaf1 {1}
        |0.2  = leaf2 {2}
        |1  = null {a}
        |""".stripMargin)
    keys.put(Array(), 1, array, "av")
    checkArray(keys, array)(
      """0/OneChild
        |0.0/Many(1)
        |0.0[0].0/OneChild
        |0.0[0].0.0  = dataInList0Child {10}
        |0.0[0].1  = dataInList0Leaf {n}
        |0.1  = leaf1 {1}
        |0.2  = leaf2 {2}
        |1  = av {a}""".stripMargin)

  }

  it should "not allow replacing of an array or a list" in {
    def checkException(block: => Unit): Unit = {
      try {
        block
        fail
      } catch {case e: IllegalStateException =>}
    }
    val keys = OrmKeys.fromList(complex)
    checkKeys(keys)(
      """b:O(),0 OneChild
        | 3:O(0),0 ManyChildren
        |  m:O(0,0),0 OneChild
        |   10:S(0,0,0),0 NoChildren
        |  n:S(0,0),1 NoChildren
        | 1:S(0),1 NoChildren
        | 2:S(0),2 NoChildren
        |a:S(),1 NoChildren""".stripMargin)
    val array = keys.makeAndSetupArray
    checkException(keys.put(Array(), 0, array, "someData"))
    checkException(keys.put(Array(0), 0, array, "someData"))
  }

  behavior of "findKey"

  it should "find the key for the index" in {
    val keys = OrmKeys.fromList(complex)
    def check(path: Array[Int], expectedPath: List[Int], expectedIndex: Int, expectedKey: String) {
      val key = keys.findKeyForPath(path)
      key.path shouldBe expectedPath
      key.index shouldBe expectedIndex
      key.key shouldBe expectedKey
    }
    //reminder of structure
    checkKeys(OrmKeys.fromList(complex))(
      """b:O(),0 OneChild
        | 3:O(0),0 ManyChildren
        |  m:O(0,0),0 OneChild
        |   10:S(0,0,0),0 NoChildren
        |  n:S(0,0),1 NoChildren
        | 1:S(0),1 NoChildren
        | 2:S(0),2 NoChildren
        |a:S(),1 NoChildren""".stripMargin)
    check(Array(0), List(), 0, "b")
    check(Array(0, 0), List(0), 0, "3")
    check(Array(0, 1), List(0), 1, "1")
    check(Array(0, 2), List(0), 2, "2")
    check(Array(0, 0, 0), List(0, 0), 0, "m")
    check(Array(0, 0, 1), List(0, 0), 1, "n")
    check(Array(1), List(), 1, "a")
  }

  behavior of "next"

  it should "add new arrays" in {
    val keys = OrmKeys.fromList(complex)
    val array = keys.makeAndSetupArray
    keys.next(Array(0, 0), array)
    checkArray(keys, array)(
      """0/OneChild
        |0.0/Many(1)
        |0.0[0].0/OneChild
        |0.0[0].0.0  = null {10}
        |0.0[0].1  = null {n}
        |0.1  = null {1}
        |0.2  = null {2}
        |1  = null {a}""".stripMargin)
    keys.put(Array(0, 0, 0), 0, array, "ShouldChangeHeadOfList")
    checkArray(keys, array)(
      """0/OneChild
        |0.0/Many(1)
        |0.0[0].0/OneChild
        |0.0[0].0.0  = ShouldChangeHeadOfList {10}
        |0.0[0].1  = null {n}
        |0.1  = null {1}
        |0.2  = null {2}
        |1  = null {a}""".stripMargin)
    keys.next(Array(0, 0), array)
    keys.put(Array(0, 0, 0), 0, array, "AndAgain")
    checkArray(keys, array)(
      """0/OneChild
        |0.0/Many(2)
        |0.0[0].0/OneChild
        |0.0[0].0.0  = AndAgain {10}
        |0.0[0].1  = null {n}
        |0.0[1].0/OneChild
        |0.0[1].0.0  = ShouldChangeHeadOfList {10}
        |0.0[1].1  = null {n}
        |0.1  = null {1}
        |0.2  = null {2}
        |1  = null {a}""".stripMargin)

  }
  it should "not allow next to be called if the path isn't a many child" in {
    val keys = OrmKeys.fromList(complex)
    val array = keys.makeAndSetupArray
    def check(path: Int*) = try {
      keys.next(path.toArray, array)
      fail()
    } catch {case e: IllegalArgumentException =>}
    check(1)
    check(1, 0)
    keys.next(Array(0, 0), array)
    check(0, 0, 1)
  }


  behavior of "NumericKeys/Json"

  def checkJson[Context, Schema[_]](context: Context, keys: OrmKeys[Schema], array: Array[Any], expected: String)(implicit ormKeys: OrmKeysToJson[Context, Schema]): Unit = {
    val stream = new ByteArrayOutputStream()
    keys.writeJsonPrimitive(context, array, stream)
    checkStrings(stream.toString(), expected)
  }
  it should "turn an array into json" in {
    val keys = OrmKeys.fromList(List(itema, itemb, itemc))
    checkKeys(keys)(
      """c:L(),0 NoChildren
        |a:S(),1 NoChildren
        |b:S(),2 NoChildren""".stripMargin)
    val array = keys.makeAndSetupArray
    keys.put(Array(), 0, array, 1.2)
    keys.put(Array(), 1, array, 123)
    keys.put(Array(), 2, array, "someData")
    checkArray(keys, array)(
      """0  = 1.2 {c}
        |1  = 123 {a}
        |2  = someData {b}""".stripMargin)

    checkJson("someContext", keys, array, """{"_links":{"c":1.2},"a":"123","b":someData}""")
  }

  it should "handle nulls" in {
    val keys = OrmKeys.fromList(List(itema, itemb, itemc))
    val array = keys.makeAndSetupArray
    checkArray(keys, array)(
      """0  = null {c}
        |1  = null {a}
        |2  = null {b}""".stripMargin)
    checkJson("someContext", keys, array, """{"_links":{"c":null},"a":null,"b":null}""")
  }

  it should "escape characters" in {
    val keys = OrmKeys.fromList(List(itema))
    val array = keys.makeAndSetupArray
    keys.put(Array(), 0, array, "so\\me \b \f \n  \r  \t  va\"lue")
    checkArray(keys, array)("""0  = so\\me \b \f \n  \r  \t  va\"lue {a}""".stripMargin)
    checkJson("someContext", keys, array, """{"a":"so\\me \b \f \n  \r  \t  va\"lue"}""")
    //    escaped = escaped.replace("\\", "\\\\")
    //    escaped = escaped.replace("\"", "\\\"")
    //    escaped = escaped.replace("\b", "\\b")
    //    escaped = escaped.replace("\f", "\\f")
    //    escaped = escaped.replace("\n", "\\n")
    //    escaped = escaped.replace("\r", "\\r")
    //    escaped = escaped.replace("\t", "\\t")
  }


  it should "turn an array with child objects into json" in {
    val keys = OrmKeys.fromList(List(itema, bcAsSingleton))
    val array = keys.makeAndSetupArray
    keys.put(Array(), 1, array, "av")
    keys.put(Array(0), 0, array, 123)
    keys.put(Array(0), 1, array, 2.1)
    checkArray(keys, array)(
      """0/OneChild
        |0.0  = 123 {c}
        |0.1  = 2.1 {b}
        |1  = av {a}""".stripMargin)
    checkJson("someContext", keys, array, """{"bc":{"_links":{"c":123},"b":2.1},"a":"av"}""")
  }

  it should "turn an array with child arrays into json" in {
    val keys = OrmKeys.fromList(List(itema, bcAsMany))
    checkKeys(keys)(
      """bc:O(),0 ManyChildren
        | c:L(0),0 NoChildren
        | b:S(0),1 NoChildren
        |a:S(),1 NoChildren""".stripMargin)
    val array = keys.makeAndSetupArray
    keys.put(Array(), 1, array, "av")
    checkArray(keys, array)(
      """0/Many(0)
        |1  = av {a}""".stripMargin)
    checkJson("someContext", keys, array, """{"bc":[],"a":"av"}""")

    keys.next(Array(0), array)
    keys.put(Array(0), 0, array, 123)
    keys.put(Array(0), 1, array, 2.3)
    checkJson("someContext", keys, array, """{"bc":[{"_links":{"c":123},"b":2.3}],"a":"av"}""")

    keys.next(Array(0), array)
    keys.put(Array(0), 0, array, 234)
    keys.put(Array(0), 1, array, 3.4)

    checkArray(keys, array)(
      """0/Many(2)
        |0[0].0  = 234 {c}
        |0[0].1  = 3.4 {b}
        |0[1].0  = 123 {c}
        |0[1].1  = 2.3 {b}
        |1  = av {a}""".stripMargin)
    checkJson("someContext", keys, array, """{"bc":[{"_links":{"c":123},"b":2.3},{"_links":{"c":234},"b":3.4}],"a":"av"}""")
  }
}
