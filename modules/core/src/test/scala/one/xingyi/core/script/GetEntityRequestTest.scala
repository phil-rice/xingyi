package one.xingyi.core.script
import one.xingyi.core.UtilsSpec
import one.xingyi.core.builder.HasId

class GetEntityRequestTest extends UtilsSpec {

  behavior of "GetEntityRequest"

  val reqNoHeader = GetEntityRequest("someId", "somehost", None)
  val reqHeader = GetEntityRequest("someId", "somehost", Some("xingyiheader"))

  val hasId = implicitly[HasId[GetEntityRequest, String]]
  val hasHost = implicitly[HasHost[GetEntityRequest]]
  val toContentType = implicitly[ToContentType[GetEntityRequest]]

  it should "have a 'HasId'" in {
    hasId(reqHeader) shouldBe "someId"
  }

  it should "have a 'has host'" in {
    hasHost(reqHeader) shouldBe "somehost"
  }

  it should "have a 'toContentType'" in {
    toContentType(reqHeader) shouldBe "xingyiheader"
    toContentType(reqNoHeader) shouldBe "application/xingyi."
  }

}
