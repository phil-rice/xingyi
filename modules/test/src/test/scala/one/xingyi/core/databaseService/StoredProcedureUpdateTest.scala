/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.databaseService

import one.xingyi.core.http._
import one.xingyi.core.json._
import one.xingyi.core.monad.IdentityMonad
import one.xingyi.core.strings.Strings
import one.xingyi.core.{FunctionFixture, UtilsSpec}
import one.xingyi.json4s.Json4sParser._
import one.xingyi.json4s.Json4sWriter._

import scala.util.Success

class StoredProcedureUpdateTest extends UtilsSpec with FunctionFixture with DatabaseFixture with JsonWriterLanguage {

  val fromServiceRequestForUpdate = implicitly[FromServiceRequest[IdentityMonad, UpdateRequest]]
  def toServiceResponseForUpdate = implicitly[ToServiceResponse[UpdateRequest, UpdateResponse]]


  behavior of "StoredProcedureUpdate"
  it should "have a FromServiceRequest" in {
    fromServiceRequestForUpdate(sr("/somePath/name", """{"one": "valueOne"}""")).value shouldBe Success(UpdateRequest("name", Map("one" -> "valueOne")))
  }


  it should "should have a toServiceResponse that return the count of the inserted rows with a status 200 (fix to 201 later) and a json object of representing the count for ints" in {
    val expectedBody = jsonWriter(JsonObject("count" -> 1))
    Strings.removeWhiteSpace(expectedBody) shouldBe """{"count":1}"""
    toServiceResponseForUpdate(UpdateRequest("name", Map("one" -> "valueOne")))(UpdateResponse(1)) shouldBe
      ServiceResponse(Status(200), Body(expectedBody), ContentType("application/json")) //TODO should really be 201.
  }


}
