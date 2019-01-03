package one.xingyi.core.script
import one.xingyi.core.UtilsSpec

class DomainAndMethodsToDisplayDomTest extends UtilsSpec with ScriptFixture {

  behavior of "DefaultDomainDefnToDisplayDom"

  val toDisplayDom = implicitly[DomainAndMethodsToDisplayDom]
  it should "have a smokeTest" in {
    toDisplayDom(domAndMethods1) shouldBe domainDd1
    toDisplayDom(domAndMethods2) shouldBe domainDd2
  }
}
