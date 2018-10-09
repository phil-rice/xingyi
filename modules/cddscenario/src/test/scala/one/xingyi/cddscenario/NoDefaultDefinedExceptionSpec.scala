package one.xingyi.cddscenario
import one.xingyi.core.UtilsSpec

class NoDefaultDefinedExceptionSpec extends UtilsSpec {

  behavior of "NoDefaultDefinedException"

  it should "have a 'throw with' that is never defined" in {
    NoDefaultDefinedException.throwWith[Int] isDefinedAt 1 shouldBe false
    NoDefaultDefinedException.throwWith[Int] isDefinedAt 2 shouldBe false
//    intercept[NoDefaultDefinedException](NoDefaultDefinedException.throwWith[Int] apply 2)
  }

}
