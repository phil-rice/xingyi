package one.xingyi.core.databaseService

import one.xingyi.core.FunctionFixture
import one.xingyi.core.databaseService.{DatabaseServiceImplicits, UpdateRequest, UpdateResponse}
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
