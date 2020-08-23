/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.databaseService

import one.xingyi.core.cheapApi.CheapApi
import one.xingyi.core.endpoint.MatchesServiceRequest
import one.xingyi.core.http._
import one.xingyi.core.json.{JsonWriter, _}
import one.xingyi.core.logging.{LoggingAdapter, NullLoggingAdapter, PrintlnLoggingAdapter}
import one.xingyi.core.monad.{Async, MonadCanFailWithException}
import one.xingyi.core.simpleServer.CheapServer

import scala.language.{higherKinds, postfixOps}


object DatabaseApi {

  implicit val toJsonLibForMapStringString: ToJsonLib[Map[String, String]] = m => JsonObject(m.map { case (k, v) => k -> JsonString(v) }.toList: _*)

  def server[M[_] : Async, Fail: Failer, J: JsonParser : JsonWriter](port: Port,
                                                                     updateKleisli: SPKleisli[M, UpdateRequest, UpdateResponse],
                                                                     queryKleisli: SPKleisli[M, QueryRequest, QueryResponse],
                                                                     storedProcKleisli: SPKleisli[M, StoredProcedureRequest, StoredProcedureResponse],
                                                                     stopAtEnd: Boolean = true, print: Boolean = true)
                                                                    (block: => Unit)
                                                                    (implicit monad: MonadCanFailWithException[M, Fail]): Unit = {
    implicit val loggingAdapter: LoggingAdapter = if (print) PrintlnLoggingAdapter else NullLoggingAdapter
    CheapServer(port, new DatabaseApi(updateKleisli, queryKleisli, storedProcKleisli).endpoints, stopAtEnd)(block)
  }
}


class DatabaseApi[M[_] : Async, Fail: Failer, J: JsonParser : JsonWriter](updateKleisli: SPKleisli[M, UpdateRequest, UpdateResponse],
                                                                          queryKleisli: SPKleisli[M, QueryRequest, QueryResponse],
                                                                          storedProcKleisli: SPKleisli[M, StoredProcedureRequest, StoredProcedureResponse])
                                                                         (implicit monad: MonadCanFailWithException[M, Fail], loggingAdapter: LoggingAdapter)
  extends CheapApi[M, Fail, J] with DatabaseServiceImplicits {
  val updateEndPoint = updateKleisli |+| endpoint[UpdateRequest, UpdateResponse]("/update", MatchesServiceRequest.idAtEnd(Post))
  val queryEndPoint = queryKleisli |+| endpoint[QueryRequest, QueryResponse]("/query", MatchesServiceRequest.idAtEnd(Post))
  val storedEndPoint = storedProcKleisli |+| endpoint[StoredProcedureRequest, StoredProcedureResponse]("/storedProcedure", MatchesServiceRequest.idAtEnd(Post))
  val endpoints: ServiceRequest => M[Option[ServiceResponse]] = chain(updateEndPoint, queryEndPoint, storedEndPoint)
}


