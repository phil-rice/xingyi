package one.xingyi.core.databaseService

import one.xingyi.core.http.{Body, Get, ServiceRequest, Uri}
import one.xingyi.core.json.{JsonParser, JsonWriter}
import one.xingyi.json4s.Json4sParser._
import one.xingyi.json4s.Json4sWriter._
import org.json4s.JValue
import org.scalatestplus.mockito.MockitoSugar

trait DatabaseFixture extends MockitoSugar {
  val jsonWriter = implicitly[JsonWriter[JValue]]
  val jsonParser = implicitly[JsonParser[JValue]]

  def sr(path: String, json: String) = ServiceRequest(Get, Uri(path), Seq(), Some(Body(json)))


//  def setupStoredProcedureKleisli[M[_] : Liftable, T]
//  (fn: (CallableStatement, QueryRequest => M[QueryResponse[Map[String, String]]], UpdateRequest => M[UpdateResponse]) => Unit): Unit = {
//    implicit val dataSource = mock[DataSource]
//    implicit val conn = mock[Connection]
//    implicit val statement = mock[CallableStatement]
//    implicit val factory = mock[CalledStatementFactory]
//    Mockito.when(dataSource.getConnection).thenReturn(conn)
//    Mockito.when(factory.apply(conn, "someSql")).thenReturn(statement)
//    val query = mock[QueryRequest => M[QueryResponse[Map[String, String]]]]
//    val update = mock[UpdateRequest => M[UpdateResponse]]
//    fn(statement, query, update)
//  }
}
