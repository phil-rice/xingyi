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

class StoredProcedureTest extends FlatSpec with FunctionFixture with DatabaseFixture with JsonWriterLanguage with DatabaseServiceImplicits {
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
