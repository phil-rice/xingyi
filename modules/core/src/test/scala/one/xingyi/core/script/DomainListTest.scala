package one.xingyi.core.script
import one.xingyi.core.UtilsSpec
import org.mockito.Mockito._
class DomainListTest extends UtilsSpec with ScriptFixture {

  behavior of "DomainList"

  it should "aggregate the passed in domains" in {
    domainList.domains shouldBe List(details1, details2)
  }

  behavior of "DomainList selecting the right details"

  it should "select the first if no lens are specified" in {
    implicit val toLensNames = mock[IXingYiHeaderToLensNames]
    when(toLensNames.apply(Some("someAcceptHeader"))) thenReturn Set[String]()
    domainList.accept(Some("someAcceptHeader")) shouldBe details1
  }

  it should "select the first if it matches" in {
    implicit val toLensNames = mock[IXingYiHeaderToLensNames]
    when(toLensNames.apply(Some("someAcceptHeader"))) thenReturn details1.lensNames
    domainList.accept(Some("someAcceptHeader")) shouldBe details1
  }

  it should "select the second if the first doesn't match but the second does" in {
    implicit val toLensNames = mock[IXingYiHeaderToLensNames]
    when(toLensNames.apply(Some("someAcceptHeader"))) thenReturn details2.lensNames
    domainList.accept(Some("someAcceptHeader")) shouldBe details2
  }

  it should "blow up if it can't match" in {
    implicit val toLensNames = mock[IXingYiHeaderToLensNames]
    when(toLensNames.apply(Some("someAcceptHeader"))) thenReturn Set("some", "rubbish", "names")
    intercept[CannotRespondToQuery](domainList.accept(Some("someAcceptHeader"))).getMessage.noWhiteSpace shouldBe
      """Header[Some(someAcceptHeader)]
        | normalised[names,rubbish,some],
        | headerAsSet: some,rubbish,names
        | failures:
        | Domain ParentDomainForTest1
        |   Allowed: lens_house_postcode_string,lens_parent_house_house,lens_parent_children_childlist,lens_parent_name_string,lens_child_name_string
        |   Failed: some,rubbish,names
        |;Domain ParentDomainForTest2
        |   Allowed: lens_house_postcode_string,lens_parent_house_house,lens_person_postcode_string,lens_parent_children_childlist,lens_parent_name_string,lens_child_name_string
        |   Failed: some,rubbish,names""".stripMargin.noWhiteSpace
  }

}
