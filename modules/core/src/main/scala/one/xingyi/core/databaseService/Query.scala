/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.databaseService

import one.xingyi.core.http._
import one.xingyi.core.json.JsonLanguage._
import one.xingyi.core.json.{FromJsonLib, JsonParser, JsonWriter, ToJsonLib}
import one.xingyi.core.language.AnyLanguage._
import one.xingyi.core.monad.Monad
import one.xingyi.core.strings.Strings

import scala.language.higherKinds


case class QueryRequest(name: String, map: Map[String, String] = Map()) extends DatabaseRequest

object QueryRequest {
  import DatabaseServiceImplicits._
  implicit def fromServiceRequestForQueryRequest[M[_] : Monad, J](implicit parser: JsonParser[J]): FromServiceRequest[M, QueryRequest] =
    sr => QueryRequest(Strings.lastSection("/")(sr.path.path), parser(sr.body.getOrElse(throw DatabaseRequest.needException(s"No Body in ServiceRequest $sr")).s).as[Map[String, String]]).liftM[M]

  implicit def toServiceRequestForQueryRequest[J](implicit jsonWriter: JsonWriter[J], mapToJson: ToJsonLib[Map[String, String]]): ToServiceRequest[QueryRequest] =
    req => ServiceRequest(Post, Uri(s"/query/${req.name}"), Seq(), Some(Body(jsonWriter(mapToJson(req.map)))))

}

case class QueryResponse(results: QueryResults)

object QueryResponse {
  def oneRow(nameAndValues: (String, String)*): QueryResponse = QueryResponse(QueryResults.oneRow(nameAndValues: _*))
  implicit def toJsonLibForStoredQueryResponse(implicit toJsonLib: ToJsonLib[QueryResults]): ToJsonLib[QueryResponse] =
    response => toJsonLib(response.results)

  implicit def fromJsonLibForStoredQueryResponse[J](implicit fromJsonLib: FromJsonLib[J, QueryResults]): FromJsonLib[J, QueryResponse] =
    j => QueryResponse(fromJsonLib.apply(j))

}
