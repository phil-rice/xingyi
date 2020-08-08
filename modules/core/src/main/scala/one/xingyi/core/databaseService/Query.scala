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