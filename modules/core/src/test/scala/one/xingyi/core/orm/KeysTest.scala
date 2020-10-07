package one.xingyi.core.orm

import one.xingyi.core.UtilsSpec
import one.xingyi.core.orm.FieldType.{int, string}

class KeysTest extends UtilsSpec {

  behavior of "Keys"

  it should "parse a primary key defnition using the notation a,b,c or a:t1,b:t2 etc" in {
    Keys("a") shouldBe Keys(List(string("a")))
    Keys("a:int") shouldBe Keys(List(int("a")))
    Keys("a,b:int") shouldBe Keys(List(string("a"), int("b")))
    Keys("a , b:int") shouldBe Keys(List(string("a"), int("b")))
    Keys(" a , b : int ") shouldBe Keys(List(string("a"), int("b")))
  }

}
