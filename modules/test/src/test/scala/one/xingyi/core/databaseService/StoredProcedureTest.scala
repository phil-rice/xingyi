/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.databaseService

import one.xingyi.core.http._
import one.xingyi.core.json._
import one.xingyi.core.monad.IdentityMonad
import one.xingyi.core.strings.Strings
import one.xingyi.core.{FunctionFixture, UtilsSpec}
import one.xingyi.json4s.Json4sParser._
import one.xingyi.json4s.Json4sWriter._
import org.json4s.JValue

import scala.util.Success

class StoredProcedureTest extends UtilsSpec with FunctionFixture with DatabaseFixture with JsonWriterLanguage with DatabaseServiceImplicits {
  val storedProcedureResponse = StoredProcedureResponse(QueryResults(List("a", "b"), List(List("1", "2"), List("11", "12"))))
  val storedProcedureResponseJson = """{"names":["a","b"],"values":[["1","2"],["11","12"]]}"""



  val fromServiceRequestForStoredProcedure = implicitly[FromServiceRequest[IdentityMonad, StoredProcedureRequest]]

  def toServiceResponseForStoredProcedure: ToServiceResponse[StoredProcedureRequest, StoredProcedureResponse] =
    implicitly[ToServiceResponse[StoredProcedureRequest, StoredProcedureResponse]]


  behavior of "StoredProcedureRequest"

  it should "have a FromServiceRequest" in {
    fromServiceRequestForStoredProcedure(sr("/somePath/name", """{"one": "valueOne"}""")).value shouldBe Success(StoredProcedureRequest("name", Map("one" -> "valueOne")))
  }


  behavior of "StoredProcedureResponse"

  it should "should have a toServiceResponse that return the results json with a status 200  " in {
    toServiceResponseForStoredProcedure(StoredProcedureRequest("name", Map("one" -> "valueOne")))(storedProcedureResponse) shouldBe
      ServiceResponse(Status(200), Body(jsonWriter(storedProcedureResponse)), ContentType("application/json"))
  }

  it should "have a fromJsonLib" in {
    implicitly[FromJsonLib[JValue, StoredProcedureResponse]].apply(storedProcedureResponseJson) shouldBe storedProcedureResponse
  }
  it should "have a toJsonLib " in {
    Strings.removeWhiteSpace(implicitly[ToJsonLib[StoredProcedureResponse]].apply(storedProcedureResponse)) shouldBe storedProcedureResponseJson
  }

}
