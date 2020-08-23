/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.databaseService

import one.xingyi.core.FunctionFixture

import one.xingyi.core.http._
import one.xingyi.core.json._
import one.xingyi.core.monad.IdentityMonad
import one.xingyi.json4s.Json4sParser._
import one.xingyi.json4s.Json4sWriter._
import org.json4s.JValue
import org.scalatest.FlatSpec

import scala.util.Success

class UpdateTest extends FlatSpec with FunctionFixture with DatabaseFixture with JsonWriterLanguage with DatabaseServiceImplicits{
  val fromServiceRequestForUpdate = implicitly[FromServiceRequest[IdentityMonad, UpdateRequest]]

  def toServiceResponseForUpdate[T: ToJsonLib]: ToServiceResponse[UpdateRequest, UpdateResponse] =
    implicitly[ToServiceResponse[UpdateRequest, UpdateResponse]]


  behavior of "UpdateRequest"

  it should "have a FromServiceRequest" in {
    fromServiceRequestForUpdate(sr("/somePath/name", """{"one": "valueOne"}""")).value shouldBe Success(UpdateRequest("name", Map("one" -> "valueOne")))
  }

  it should "should have a toServiceResponse that return the json '{'count': #}'  with a status 200  " in {
    toServiceResponseForUpdate[Map[String, String]].apply(UpdateRequest("name", Map("one" -> "valueOne")))(UpdateResponse(123)) shouldBe
      ServiceResponse(Status(200), Body(jsonWriter(JsonObject("count" -> 123))), ContentType("application/json"))
  }

  behavior of "UpdateResponse"

  it should "have a fromJsonLib that processes json object count:#" in {
    val json = implicitly[JsonParser[JValue]].apply("""{"count":123}""")
    implicitly[FromJsonLib[JValue, UpdateResponse]].apply(json) shouldBe UpdateResponse(123)
  }
  it should "have a toJsonLib that produces count:#" in {
    implicitly[ToJsonLib[UpdateResponse]].apply(UpdateResponse(123)) shouldBe JsonObject(("count", JsonInt(123)))
  }

}
