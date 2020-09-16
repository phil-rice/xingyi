package one.xingyi.core.orm

import one.xingyi.core.UtilsSpec
import one.xingyi.core.orm.FieldType.{int, string}

class FieldTypeTest extends UtilsSpec {

  behavior of "parse"

  it should "allow strings to be turned into IntField" in {
    FieldType("one:int") shouldBe int("one")
    FieldType("one: int") shouldBe int("one")
    FieldType("one : int") shouldBe int("one")
  }
  it should "allow strings to be turned into StringField" in {
    FieldType("one:string") shouldBe string("one")
    FieldType("one: string") shouldBe string("one")
    FieldType("one : string") shouldBe string("one")
  }

  it should "allow default to  StringField" in {
    FieldType("one") shouldBe string("one")
  }

  it should "allow throw exception if illegal syntax" in {
    the[RuntimeException] thrownBy (FieldType("one:abdc")) should have message ("Cannot work out what type of field one is. Its type is [abdc] and not int or string")
  }

}
