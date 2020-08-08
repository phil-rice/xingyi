package one.xingyi.core.databaseService

import one.xingyi.core.cheapApi.CheapApi
import one.xingyi.core.endpoint.MatchesServiceRequest
import one.xingyi.core.http._
import one.xingyi.core.json.{JsonWriter, _}
import one.xingyi.core.logging.{LoggingAdapter, NullLoggingAdapter, PrintlnLoggingAdapter}
import one.xingyi.core.monad.{Async, MonadCanFailWithException}
import one.xingyi.core.simpleServer.CheapServer

import scala.language.{higherKinds, postfixOps}

trait DatabaseRequest {
  def name: String
  def map: Map[String, String]
}

object DatabaseRequest {
  def needException(msg: String) = new RuntimeException(s"body of request must include a Json object. $msg")
}

object StoredProcedureApi {

  implicit val toJsonLibForMapStringString: ToJsonLib[Map[String, String]] = m => JsonObject(m.map { case (k, v) => k -> JsonString(v) }.toList: _*)

  def server[M[_] : Async, Fail: Failer, J: JsonParser : JsonWriter](port: Port,
                                                                     updateKleisli: SPKleisli[M, UpdateRequest, UpdateResponse],
                                                                     queryKleisli: SPKleisli[M, QueryRequest, QueryResponse],
                                                                     storedProcKleisli: SPKleisli[M, StoredProcedureRequest, StoredProcedureResponse],
                                                                     stopAtEnd: Boolean = true, print: Boolean = true)
                                                                    (block: (StoredProcedureApi[M, Fail, J]) => Unit)
                                                                    (implicit monad: MonadCanFailWithException[M, Fail]): Unit = {
    implicit val loggingAdapter: LoggingAdapter = if (print) PrintlnLoggingAdapter else NullLoggingAdapter
    val api: StoredProcedureApi[M, Fail, J] = new StoredProcedureApi[M, Fail, J](updateKleisli, queryKleisli, storedProcKleisli)
    CheapServer(port, api.endpoints, stopAtEnd)(block(api))
  }
}


class StoredProcedureApi[M[_] : Async, Fail: Failer, J: JsonParser : JsonWriter](updateKleisli: SPKleisli[M, UpdateRequest, UpdateResponse],
                                                                                 queryKleisli: SPKleisli[M, QueryRequest, QueryResponse],
                                                                                 storedProcKleisli: SPKleisli[M, StoredProcedureRequest, StoredProcedureResponse])
                                                                                (implicit monad: MonadCanFailWithException[M, Fail], loggingAdapter: LoggingAdapter)
  extends CheapApi[M, Fail, J] with DatabaseServiceImplicits {
  val updateEndPoint = updateKleisli |+| endpoint[UpdateRequest, UpdateResponse]("/update", MatchesServiceRequest.idAtEnd(Post))
  val queryEndPoint = queryKleisli |+| endpoint[QueryRequest, QueryResponse]("/query", MatchesServiceRequest.idAtEnd(Post))
  val storedEndPoint = storedProcKleisli |+| endpoint[StoredProcedureRequest, StoredProcedureResponse]("/storedProcedure", MatchesServiceRequest.idAtEnd(Post))
  val endpoints: ServiceRequest => M[Option[ServiceResponse]] = chain(updateEndPoint, queryEndPoint, storedEndPoint)
}


