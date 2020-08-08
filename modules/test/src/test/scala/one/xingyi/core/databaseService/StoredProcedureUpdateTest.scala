package one.xingyi.core.databaseService

import one.xingyi.core.FunctionFixture
import one.xingyi.core.databaseService.{UpdateRequest, UpdateResponse}
import one.xingyi.core.http._
import one.xingyi.core.json._
import one.xingyi.core.monad.IdentityMonad
import one.xingyi.core.strings.Strings
import one.xingyi.json4s.Json4sParser._
import one.xingyi.json4s.Json4sWriter._
import org.scalatest.FlatSpec

import scala.util.Success

class StoredProcedureUpdateTest extends FlatSpec with FunctionFixture with DatabaseFixture with JsonWriterLanguage {

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
