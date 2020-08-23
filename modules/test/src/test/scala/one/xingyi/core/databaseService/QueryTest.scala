/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.databaseService

import one.xingyi.core.FunctionFixture

import one.xingyi.core.http._
import one.xingyi.core.json._
import one.xingyi.core.monad.IdentityMonad
import one.xingyi.core.strings.Strings
import one.xingyi.json4s.Json4sParser._
import one.xingyi.json4s.Json4sWriter._
import org.json4s.JValue
import org.scalatest.FlatSpec

import scala.util.Success



class QueryTest extends FlatSpec with FunctionFixture with DatabaseFixture with JsonWriterLanguage with QueryResponseJsonFixture with DatabaseServiceImplicits{
  val fromServiceRequestForQuery = implicitly[FromServiceRequest[IdentityMonad, QueryRequest]]

  def toServiceResponseForQuery[T: ToJsonLib]: ToServiceResponse[QueryRequest, QueryResponse] =
    implicitly[ToServiceResponse[QueryRequest, QueryResponse]]


  behavior of "QueryRequest"

  it should "have a FromServiceRequest" in {
    fromServiceRequestForQuery(sr("/somePath/name", """{"one": "valueOne"}""")).value shouldBe Success(QueryRequest("name", Map("one" -> "valueOne")))
  }

  it should "should have a toServiceResponse that return the json for the resultSetFunction rows with a status 200  " in {
    toServiceResponseForQuery[Map[String, String]].apply(QueryRequest("name", Map("one" -> "valueOne")))(queryResponse) shouldBe
      ServiceResponse(Status(200), Body(jsonWriter(queryResponse)), ContentType("application/json"))
  }

  behavior of "QueryResponse"


  it should "have a fromJsonLib" in {
    implicitly[FromJsonLib[JValue, QueryResponse]].apply(responseJson) shouldBe queryResponse
  }
  it should "have a toJsonLib " in {
    Strings.removeWhiteSpace(implicitly[ToJsonLib[QueryResponse]].apply(queryResponse)) shouldBe responseJson
  }

}
