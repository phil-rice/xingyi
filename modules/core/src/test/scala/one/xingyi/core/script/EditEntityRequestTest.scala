/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.script
import one.xingyi.core.UtilsSpec
import one.xingyi.core.builder.HasId
import one.xingyi.core.http._
import one.xingyi.core.json.{JsonParser, JsonWriter, ObjectProjection, ProofOfBinding}
import one.xingyi.core.monad.IdentityMonad

import scala.util.Failure

abstract class EditEntityRequestTest[J: JsonParser, SharedE, DomainE]
(implicit hasId: HasId[DomainE, String],
 jsonWriter: JsonWriter[J],
 projection: ObjectProjection[SharedE, DomainE],
 fromServiceRequest: FromServiceRequest[IdentityMonad, EditEntityRequest[DomainE]]) extends UtilsSpec {
  def id1 = "id1"
  def idWrong = "idWrong"

  def dom: DomainE

  def json = jsonWriter(projection.toJson(dom))

  val urlPattern = "/some/uri/<id>"
  val srNoAcceptHeader = ServiceRequest(Get, Uri(urlPattern.replace("<id>", id1)), Seq(Header("Host", "someHost")), Some(Body(json)))
  val srAcceptHeader = ServiceRequest(Get, Uri(urlPattern.replace("<id>", id1)), Seq(Header("Host", "someOtherHost"), Header("Accept", "someAcceptHeader")), Some(Body(json)))
  val srWrongId = ServiceRequest(Get, Uri(urlPattern.replace("<id>", idWrong)), Seq(Header("Host", "someOtherHost"), Header("Accept", "someAcceptHeader")), Some(Body(json)))
  val srNoBody = ServiceRequest(Get, Uri(urlPattern.replace("<id>", id1)), Seq(Header("Host", "someOtherHost"), Header("Accept", "someAcceptHeader")), None)
  val srNoHost = ServiceRequest(Get, Uri(urlPattern.replace("<id>", id1)), Seq(Header("Accept", "someAcceptHeader")), Some(Body(json)))


  behavior of "EditEntityRequest fromServiceRequest"

  it should "have a happy path making the domain object, capturing the accept header and the host" in {
    fromServiceRequest(srNoAcceptHeader).value.get shouldBe EditEntityRequest(dom, None, "someHost")
    fromServiceRequest(srAcceptHeader).value.get shouldBe EditEntityRequest(dom, Some("someAcceptHeader"), "someOtherHost")
  }

  it should "fail if the id in the url doesn't match the id in the json" in {
    val Failure(e) = fromServiceRequest(srWrongId).value
    e.getMessage shouldBe s"Id mismatch in url idWrong\n$srWrongId"

  }

  it should "fail if there is no json" in {
    val Failure(e) = fromServiceRequest(srNoBody).value
    e.getMessage shouldBe s"No json in the request\n$srNoBody"
  }

  it should "fail if there is no host" in {
    val Failure(e) = fromServiceRequest(srNoHost).value
    e.getMessage shouldBe s"""No host in the request\n$srNoHost"""
  }
  behavior of "EditEntityRequest"

  it should "delegate hasId to the dom" in {
    val req = fromServiceRequest(srNoAcceptHeader).value.get
    implicitly[HasId[EditEntityRequest[DomainE], String]] apply req shouldBe id1
  }

  it should "have a hasHost" in {
    val reqSomeHost = fromServiceRequest(srNoAcceptHeader).value.get
    implicitly[HasHost[EditEntityRequest[DomainE]]] apply reqSomeHost shouldBe "someHost"
  }

  //TODO Come back and consider this. Should the accept header only have a value if it starts/has with xingyi?
  behavior of "EditEntityRequest ToContentType"
  it should "return the DomainDefn.xingyiHeaderPrefix header if no accept header" in {
    val req = fromServiceRequest(srNoAcceptHeader).value.get
    implicitly[ToContentType[EditEntityRequest[DomainE]]] apply req shouldBe DomainDefn.xingyiHeaderPrefix
  }
  it should "return the accept header if given" in {
    val req = fromServiceRequest(srAcceptHeader).value.get
    implicitly[ToContentType[EditEntityRequest[DomainE]]] apply req shouldBe "someAcceptHeader"

  }


}
