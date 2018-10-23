/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.sampleServer

import java.util.ResourceBundle
import java.util.concurrent.{Executor, Executors}

import one.xingyi.core.cache.{CacheFactory, CachingServiceFactory, DurationStaleCacheStategy}
import one.xingyi.core.http._
import one.xingyi.core.json.{JsonParser, JsonWriter}
import one.xingyi.core.language.{MicroserviceBuilder, MicroserviceComposers}
import one.xingyi.core.local.ExecutionContextWithLocal
import one.xingyi.core.logging.{DetailedLogging, LogRequestAndResult, PrintlnLoggingAdapter, SimpleLogRequestAndResult}
import one.xingyi.core.map.NoMapSizeStrategy
import one.xingyi.core.metrics.{PrintlnPutMetrics, PutMetrics}
import one.xingyi.core.monad.AsyncForScalaFuture._
import one.xingyi.core.monad.{Async, MonadCanFail, MonadCanFailWithException}
import one.xingyi.core.simpleServer.{EndpointHandler, SimpleHttpServer}
import one.xingyi.core.strings.IndentAnd
import one.xingyi.core.time.NanoTimeService
import one.xingyi.sample.{BillboardSetup, FnordSetup, VogueSetup}
import org.json4s.JsonAST.JValue

import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds

class AllProducers[M[_], J: JsonWriter : JsonParser, Fail](port: Int)(implicit executor: Executor,
                                                                      val monad: MonadCanFailWithException[M, Fail],
                                                                      val async: Async[M],
                                                                      val failer: Failer[Fail],
                                                                      val cacheFactory: CacheFactory[M],
                                                                      val timeService: NanoTimeService,
                                                                      val logReqAndResult: LogRequestAndResult[Fail],
                                                                      val putMetrics: PutMetrics,
                                                                      val httpFactory: HttpFactory[M, ServiceRequest, ServiceResponse],
                                                                      val detailedLoggingForSR: DetailedLogging[ServiceResponse]) extends MicroserviceBuilder[M, Fail] with MicroserviceComposers[M] {
  val vogueSetup = new VogueSetup
  val billboardSetup = new BillboardSetup
  val fnordSetup = new FnordSetup

  val endpoints: Seq[ServiceRequest => M[Option[ServiceResponse]]] = Seq(
    vogueSetup.mostpopularEndpoint,
    billboardSetup.billboardEndpoint,
    fnordSetup.productionEndpoint,
    fnordSetup.programmeEndpoint)
  val rawMicroservice = chain(endpoints: _*)
  val server = new SimpleHttpServer(port, new EndpointHandler[M, Fail](rawMicroservice))


}

import one.xingyi.json4s.Json4sParser._
import one.xingyi.json4s.Json4sWriter._

object AllProducersApp {
  implicit val executors = Executors.newFixedThreadPool(10)
  implicit val exc = new ExecutionContextWithLocal(ExecutionContext.fromExecutor(executors))

  implicit val httpFactory = new HttpFactory[Future, ServiceRequest, ServiceResponse] {
    override def apply(v1: ServiceName) = { req => Future.successful(ServiceResponse(Status(200), Body(s"response: ${req.body.map(_.s).getOrElse("")}"), ContentType("text/html"))) }
  }
  implicit val loggingAdapter = PrintlnLoggingAdapter
  implicit val resourceBundle = ResourceBundle.getBundle("messages")
  implicit val putMetrics = PrintlnPutMetrics
  implicit val logRequestAndResult: LogRequestAndResult[Throwable] = new SimpleLogRequestAndResult
  implicit val cacheFactory = new CachingServiceFactory[Future](DurationStaleCacheStategy(10000000000L, 10000000000000L), NoMapSizeStrategy)

  import one.xingyi.core.http.Failer.failerForThrowable

  val producers = new AllProducers(port = 9010)
  producers.server.start()
}