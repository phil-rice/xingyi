package one.xingyi.core.orm

import one.xingyi.core.UtilsSpec
import one.xingyi.core.orm.FieldType.{int, string}
import org.scalatest.FlatSpec

class KeysTest extends UtilsSpec {

  behavior of "Keys"

  it should "parse a primary key defnition using the notation a,b,c or a:t1,b:t2 etc" in {
    Keys("a") shouldBe Keys(List(string("a")))
    Keys("a:int") shouldBe Keys(List(int("a")))
    Keys("a,b:int") shouldBe Keys(List(string("a"), int("b")))
    Keys("a , b:int") shouldBe Keys(List(string("a"), int("b")))
    Keys(" a , b : int ") shouldBe Keys(List(string("a"), int("b")))
  }

  it should "be re-written for the keys with index" in {
    fail
  }
  //  it should "put data into a map, only one key if only one key in the key" in {
  //    Keys("a").asPrimaryKeyAddTo(Map("x" -> 1), List("a", "b", "c"), 123) shouldBe Map("x" -> 1, "a" -> 123)
  //  }
  //  it should "put data into a map, using a tuple of the keys if multiple keys" in {
  //    Keys("a1,a2").asPrimaryKeyAddTo(Map("x" -> 1), List("a", "b", "c"), 123) shouldBe Map("x" -> 1, List("a", "b") -> 123)
  //    Keys("a1,a2,a3").asPrimaryKeyAddTo(Map("x" -> 1), List("a", "b", "c"), 123) shouldBe Map("x" -> 1, List("a", "b", "c") -> 123)
  //  }
  //  it should "get data out of a map, when there is an offset for which is the first column of the index" in {
  //    Keys("a1,a2").getFrom(Map("x" -> 1, List("a", "b") -> 123, List("b", "c") -> 234), 0, List("a", "b", "c")) shouldBe 123
  //    Keys("a1,a2").getFrom(Map("x" -> 1, List("a", "b") -> 123, List("b", "c") -> 234), 1, List("a", "b", "c")) shouldBe 234
  //
  //  }


}
