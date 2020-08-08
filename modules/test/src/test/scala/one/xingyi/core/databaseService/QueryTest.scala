package one.xingyi.core.databaseService

import one.xingyi.core.FunctionFixture
import one.xingyi.core.databaseService.{DatabaseServiceImplicits, QueryRequest, QueryResponse, QueryResults}
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
