package one.xingyi.core.orm

import java.util.concurrent.atomic.AtomicReference

import one.xingyi.core.UtilsSpec

class OrmDataTest extends UtilsSpec {


  behavior of classOf[FlyweightKey].getSimpleName

  implicit class KeysOps(s: String) {
    def ki(is: Int*): KeysAndIndex = {
      val k = Keys(s)
      require(is.size == k.list.size)
      KeysAndIndex(is.zip(k.list).toList)
    }
  }
  it should "be created from two KeysAndIndex" in {
    FlyweightKey("kl".ki(1), "kr".ki(2)) shouldBe KeyString(1, 2)
    FlyweightKey("kl:int".ki(3), "kr:int".ki(2)) shouldBe KeyInt(3, 2)
    FlyweightKey("kl:int,kl2:string".ki(3, 4), "kr:int,kr2:string".ki(4, 1)) shouldBe Key2(KeyInt(3, 4), KeyString(4, 1))
  }

  classOf[KeyInt].getSimpleName should "compare values" in {
    KeyInt(1, 2).compare(List(0, 7, 3), List(9, 9, 7)) shouldBe 0
    KeyInt(1, 2).compare(List(0, 5, 3), List(9, 9, 7)) shouldBe -1
    KeyInt(1, 2).compare(List(0, 3, 3), List(9, 9, 7)) shouldBe -1
    KeyInt(1, 2).compare(List(0, 11, 3), List(9, 9, 7)) shouldBe 1
  }
  classOf[KeyString].getSimpleName should "compare values" in {
    KeyString(1, 2).compare(List(0, 7, 3), List(9, 9, 7)) shouldBe 0
    KeyString(1, 2).compare(List(0, 5, 3), List(9, 9, 7)) < 0 shouldBe true
    KeyString(1, 2).compare(List(0, 11, 3), List(9, 9, 7)) < 0 shouldBe true
    KeyString(1, 2).compare(List(0, 9, 3), List(9, 9, 7)) > 0 shouldBe true
  }


  behavior of FanoutOrmData.getClass.getSimpleName


  implicit def intToData(i: Int) = List(i)

  def setup(fn: (((String, List[Any]) => Unit)) => Unit): List[String] = {
    val result = new AtomicReference(List[String]())
    def remember(name: String, row: List[Any]) = result.updateAndGet(old => old :+ name + row.mkString("(", ",", ")"))
    fn(remember)
    result.get
  }
  it should "process the data in it's array in a manner similar to a merge sort - only zero or one value in child per parent" in {
    setup { (remember) =>
      val child = FanoutOrmData("child", FlyweightKey("parentKey".ki(0), "childKey".ki(0)), remember, Array[List[Any]](3, 6), List())
      val parent = MainOrmData("parent", remember, Array[List[Any]](1, 2, 3, 4, 5, 6), List(child))
      parent.applyAll()
    } shouldBe List("parent(1)", "parent(2)", "parent(3)", "child(3)", "parent(4)", "parent(5)", "parent(6)", "child(6)")

    setup { (remember) =>
      val child = FanoutOrmData("child", FlyweightKey("parentKey".ki(0), "childKey".ki(0)), remember, Array[List[Any]](1, 5), List())
      val parent = MainOrmData("parent", remember, Array[List[Any]](1, 2, 3, 4, 5, 6), List(child))
      parent.applyAll()
    } shouldBe List("parent(1)", "child(1)", "parent(2)", "parent(3)", "parent(4)", "parent(5)", "child(5)", "parent(6)")
  }
  it should "process the data in it's array in a manner similar to a merge sort - deeper" in {
    setup { (remember) =>
      val grandchild = FanoutOrmData("grandchild", FlyweightKey("parentKey".ki(1), "childKey".ki(0)), remember, Array[List[Any]](1, 2), List())
      val child = FanoutOrmData("child", FlyweightKey("parentKey".ki(0), "childKey".ki(0)), remember, Array[List[Any]](List(3, 1), List(6, 2)), List(grandchild))
      val parent = MainOrmData("parent", remember, Array[List[Any]](1, 2, 3, 4, 5, 6), List(child))
      parent.applyAll()
    } shouldBe List("parent(1)", "parent(2)", "parent(3)", "child(3,1)", "grandchild(1)", "parent(4)", "parent(5)", "parent(6)", "child(6,2)", "grandchild(2)")

  }
  it should "process the data in it's array in a manner similar to a merge sort - multiple children" in {
    setup { (remember) =>
      val grandchild = FanoutOrmData("grandchild", FlyweightKey("parentKey".ki(1), "childKey".ki(0)), remember, Array[List[Any]](1, 2, 3), List())
      val child = FanoutOrmData("child", FlyweightKey("parentKey".ki(0), "childKey".ki(0)), remember, Array[List[Any]](List(3, 1), List(3, 2), List(3, 3)), List(grandchild))
      val parent = MainOrmData("parent", remember, Array[List[Any]](1, 2, 3, 4, 5, 6), List(child))
      parent.applyAll()
    } shouldBe List("parent(1)", "parent(2)", "parent(3)", "child(3,1)", "grandchild(1)", "child(3,2)", "grandchild(2)", "child(3,3)", "grandchild(3)", "parent(4)", "parent(5)", "parent(6)")
  }

  behavior of FanInOrmData.getClass.getSimpleName

  it should "process if it's id matches the parent, and then process children" in {
    setup { (remember) =>
      val grandchild = FanoutOrmData("grandchild", FlyweightKey("parentKey".ki(0), "childKey".ki(0)), remember, Array[List[Any]](1, 2, 4), List())
      val fanIn = FanInOrmData("child", idInParentData = row => row(1), idForChild = row => row(0), remember, Array(1, 2, 4), List(grandchild))
      val parent = MainOrmData("parent", remember, Array[List[Any]](List(1, 0), List(2, 0), List(3, 1), List(4, 0), List(5, 2), List(6, 4)), List(fanIn))
      parent.applyAll()
    } shouldBe List("parent(1,0)", "parent(2,0)", "parent(3,1)", "child(1)", "grandchild(1)", "parent(4,0)", "parent(5,2)", "child(2)", "grandchild(2)", "parent(6,4)", "child(4)", "grandchild(4)")
  }
}
