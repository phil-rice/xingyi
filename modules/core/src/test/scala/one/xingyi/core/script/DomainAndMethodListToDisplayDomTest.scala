package one.xingyi.core.script
import one.xingyi.core.UtilsSpec
import one.xingyi.core.json.ToJsonLib

class DomainAndMethodListToDisplayDomTest extends UtilsSpec with ScriptFixture {

  behavior of "DomainListToDisplayDom"


  val toDisplayDom = implicitly[DomainAndMethodListToDisplayDom]
  it should "have a smokeTest" in {
    toDisplayDom(listOfDomainAndMethods1) shouldBe DomainListDD(domainDd1, List(domainDd1, domainDd2))
    toDisplayDom(listofDomainAndMethods2) shouldBe DomainListDD(domainDd2, List(domainDd1, domainDd2))
  }

  behavior of "DomainListToJson"

  val toJson =implicitly[ToJsonLib[DomainListDD]]

}
