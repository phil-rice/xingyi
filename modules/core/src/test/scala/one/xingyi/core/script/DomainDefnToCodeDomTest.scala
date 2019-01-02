package one.xingyi.core.script
import one.xingyi.core.UtilsSpec

class DomainDefnToCodeDomTest extends UtilsSpec with ScriptFixture {

  behavior of "DomainDefnToCodeDom"

  val defnToCodeDom = implicitly[DomainDefnToCodeDom]

  it should "make a code dom with just one object" in {
    defnToCodeDom(new ParentDomainForTest1) shouldBe domainCd1
  }

  it should "make a code dom when there are child objects" in {
    defnToCodeDom(new ParentDomainForTest2) shouldBe domainCd2
  }

}
