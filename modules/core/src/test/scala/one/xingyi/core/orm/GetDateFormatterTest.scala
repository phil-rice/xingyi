package one.xingyi.core.orm

import one.xingyi.core.UtilsSpec

class GetDateFormatterTest extends UtilsSpec {

  behavior of classOf[GetDateFormatter].getSimpleName

  it should "get a threadlocal with a date formatter" in {
    val formatter = implicitly[GetDateFormatter]
    val a1 = formatter("yyyy-MM-dd")
    val b1 = formatter("MM/dd/yyyy")
    val a2 = formatter("yyyy-MM-dd")
    val b2 = formatter("MM/dd/yyyy")

    a1 should be theSameInstanceAs (a2)
    b1 should be theSameInstanceAs (b2)

    a1.get.format(a1.get().parse("2020-5-1")) shouldBe "2020-05-01"
    b1.get.format(b1.get().parse("5/1/2020")) shouldBe "05/01/2020"
  }

}
