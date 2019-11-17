package one.xingyi.core.script
import one.xingyi.core.UtilsSpec
import one.xingyi.core.http._
import one.xingyi.core.json.JsonWriter
import one.xingyi.core.monad.IdentityMonad

abstract class CodeDetailsRequestTest[J: JsonWriter] extends UtilsSpec with ScriptFixture {
  behavior of "CodeDetailsRequest"

  it should "have a FromServiceRequest" in {
    implicitly[FromServiceRequest[IdentityMonad, CodeDetailsRequest]].apply(ServiceRequest(Get, Uri("/some/uri/hash"))).value.get shouldBe CodeDetailsRequest("hash")
  }

  behavior of "CodeDetailsResponse"

  it should "have a toServiceResponse" in {
    val toSr = implicitly[ToServiceResponse[CodeDetailsRequest, CodeDetailsResponse]]
    val serviceResponse = toSr(CodeDetailsRequest("someHash"))(CodeDetailsResponse("someCode", MediaType("someMediaType")))

    serviceResponse.status shouldBe Status(200)
    serviceResponse.headers shouldBe List(ContentType("someMediaType"))
    serviceResponse.body.asString shouldBe "someCode"

  }

}
