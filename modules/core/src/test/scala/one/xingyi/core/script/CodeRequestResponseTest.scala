/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
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
    response.body.s.noWhiteSpace shouldBe
      s"""[
         |{"name":"ParentDomainForTest1",
         |"code":{"Javascript":"$js0Hash",
         |"ScalaCode":"$scala0Hash"}},
         |{"name":"ParentDomainForTest2",
         |"code":{"Javascript":"$js1Hash",
         |"ScalaCode":"$scala1Hash"}}]""".stripMargin.noWhiteSpace
  }

}
