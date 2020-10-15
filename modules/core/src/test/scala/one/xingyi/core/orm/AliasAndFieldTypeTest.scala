package one.xingyi.core.orm

import one.xingyi.core.UtilsSpec

class AliasAndFieldTypeTest extends UtilsSpec {

  behavior of classOf[AliasAndFieldType[_]].getSimpleName

  it should "Have a to String" in {
    val list: List[AliasAndFieldType[_]] = AliasAndFieldType(Alias("alias1"), FieldType("f1")) :: AliasAndFieldType(Alias("aliase2"), "f2", List("f3:int"))
    AliasAndFieldType.toString(list) shouldBe "alias1: f1/varchar(255), aliase2: f2/varchar(255), f3/integer"

  }
}
