/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.databaseService

import one.xingyi.core.http._
import one.xingyi.core.json.{JsonParser, JsonWriter}
import one.xingyi.core.language.MicroserviceComposers
import one.xingyi.core.logging.DetailedLogging
import one.xingyi.core.monad.MonadCanFailWithException
import one.xingyi.core.objectify.ObjectifyKleisli

import scala.language.higherKinds
class DatabaseClient[M[_], Fail, J: JsonParser : JsonWriter](serviceName: ServiceName)
                                                            (implicit val monad: MonadCanFailWithException[M, Fail],
                                                                    val failer: Failer[Fail],
                                                                    val detailedLoggingForSR: DetailedLogging[ServiceResponse],
                                                                    val httpFactory: HttpFactory[M, ServiceRequest, ServiceResponse])
  extends HttpKlesili[M] with MicroserviceComposers[M] with ObjectifyKleisli[M, Fail]{
  val query = httpFactory(serviceName) |+| objectify[QueryRequest, QueryResponse]
  val update = httpFactory(serviceName) |+| objectify[UpdateRequest, UpdateResponse]
}

