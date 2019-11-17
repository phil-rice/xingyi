package one.xingyi.core.script
import one.xingyi.core.UtilsSpec
import one.xingyi.core.http._
import one.xingyi.core.json.JsonWriter
import one.xingyi.core.monad.IdentityMonad

abstract class CodeRequestResponseTest[J: JsonWriter] extends UtilsSpec with ScriptFixture {


  behavior of "CodeRequest"

  it should "be created from a service request" in {
    implicitly[FromServiceRequest[IdentityMonad, CodeRequest]].apply(ServiceRequest(Get, Uri("/someuri"))).value.get shouldBe CodeRequest()
  }

  behavior of "CodeResponse"

  it should "create a service Response" in {
    val toSr = implicitly[ToServiceResponse[CodeRequest, CodeResponse[IParent, ParentForTest]]]
    val response = toSr(CodeRequest())(CodeResponse(domainList))
    response.status shouldBe Status(200)
    response.headers shouldBe List(ContentType("application/json"))
    response.body.asUtf.noWhiteSpace shouldBe
      s"""[
         |{"name":"ParentDomainForTest1",
         |"code":{"Javascript":"$js0Hash",
         |"ScalaCode":"$scala0Hash"}},
         |{"name":"ParentDomainForTest2",
         |"code":{"Javascript":"$js1Hash",
         |"ScalaCode":"$scala1Hash"}}]""".stripMargin.noWhiteSpace
  }

}
