/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.databaseService

import one.xingyi.core.MockitoSugar
import one.xingyi.core.http.{Body, Get, ServiceRequest, Uri}
import one.xingyi.core.json.{JsonParser, JsonWriter}
import one.xingyi.json4s.Json4sParser._
import one.xingyi.json4s.Json4sWriter._
import org.json4s.JValue

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
