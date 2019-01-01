package one.xingyi.core.script
import one.xingyi.core.UtilsSpec

class IXingYiHeaderToLensNamesTest extends UtilsSpec {

  val toLensnames = implicitly[IXingYiHeaderToLensNames]

  behavior of "IXingYiHeaderToLensNames"

  it should "return an empty set if there is no accept header" in {
    toLensnames(None) shouldBe Set()
  }

  it should "return an empty set if there is an accept header but no application/xingyi...." in {
    toLensnames(Some("not interesting stuff")) shouldBe Set()
  }

  it should "blow up if the string with appliction/axingyi isn't at the start" in {
    intercept[RuntimeException](toLensnames(Some("someprefix application/xingyi.lens_person_line1_string,lens_person_line2_string")))

  }
  it should "rip apart a valid application/xingyi header" in {
    toLensnames(Some("application/xingyi.lens_person_line1_string,lens_person_line2_string")) shouldBe Set("lens_person_line1_string", "lens_person_line2_string")
  }
}
