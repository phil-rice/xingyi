package one.xingyi.core.databaseService

import one.xingyi.core.databaseService.DatabaseServiceImplicits._
import one.xingyi.core.http._
import one.xingyi.core.json.{JsonParser, JsonWriter}
import one.xingyi.core.language.MicroserviceComposers
import one.xingyi.core.logging.DetailedLogging
import one.xingyi.core.monad.MonadCanFailWithException
import one.xingyi.core.objectify.ObjectifyKleisli

import scala.language.higherKinds
class StoredProcedureClient[M[_], Fail, J: JsonParser : JsonWriter](serviceName: ServiceName)
                                                                   (implicit val monad: MonadCanFailWithException[M, Fail],
                                                                    val failer: Failer[Fail],
                                                                    val detailedLoggingForSR: DetailedLogging[ServiceResponse],
                                                                    val httpFactory: HttpFactory[M, ServiceRequest, ServiceResponse])
  extends HttpKlesili[M] with MicroserviceComposers[M] with ObjectifyKleisli[M, Fail]{
  val query = httpFactory(serviceName) |+| objectify[QueryRequest, QueryResponse]
  val update = httpFactory(serviceName) |+| objectify[UpdateRequest, UpdateResponse]
}

