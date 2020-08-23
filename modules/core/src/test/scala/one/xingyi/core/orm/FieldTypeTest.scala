package one.xingyi.core.orm

import one.xingyi.core.UtilsSpec

class FieldTypeTest extends UtilsSpec {

  behavior of "parse"

  it should "allow strings to be turned into IntField" in {
    FieldType.parse("one:int") shouldBe IntField("one")
    FieldType.parse("one: int") shouldBe IntField("one")
    FieldType.parse("one : int") shouldBe IntField("one")
  }
  it should "allow strings to be turned into StringField" in {
    FieldType.parse("one:string") shouldBe StringField("one")
    FieldType.parse("one: string") shouldBe StringField("one")
    FieldType.parse("one : string") shouldBe StringField("one")
  }

  it should "allow throw exception if illegal syntax" in {
    the[RuntimeException] thrownBy (FieldType.parse("one")) should have message ("Cannot split a string into two non empty parts using [:] string was [one]")
    the[RuntimeException] thrownBy (FieldType.parse("one:abdc")) should have message ("Cannot work out what type of field one is. Its type is [abdc] and not int or string")
  }

}
