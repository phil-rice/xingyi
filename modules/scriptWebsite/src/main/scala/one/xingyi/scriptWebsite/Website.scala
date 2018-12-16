/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.scriptWebsite

import java.util.ResourceBundle
import java.util.concurrent.Executors

import javax.net.ssl.SSLContext
import one.xingyi.core.aggregate.{EnrichLanguage, MergeLanguage}
import one.xingyi.core.cache.{CacheFactory, CachingServiceFactory, DurationStaleCacheStategy}
import one.xingyi.core.client.HttpClient
import one.xingyi.core.http._
import one.xingyi.core.json.{JsonParser, JsonWriter}
import one.xingyi.core.language._
import one.xingyi.core.logging.{DetailedLogging, LogRequestAndResult, PrintlnLoggingAdapter, SimpleLogRequestAndResult}
import one.xingyi.core.map.NoMapSizeStrategy
import one.xingyi.core.metrics.{PrintlnPutMetrics, PutMetrics}
import one.xingyi.core.monad.{Async, IdentityMonad, MonadCanFailWithException}
import one.xingyi.core.simpleServer.{EndpointHandler, SimpleHttpServer}
import one.xingyi.core.time.NanoTimeService
import one.xingyi.json4s.Json4sParser._
import one.xingyi.json4s.Json4sWriter._
import org.json4s.JValue

import scala.language.higherKinds


class Website[M[_], Fail, J: JsonParser : JsonWriter](implicit val monad: MonadCanFailWithException[M, Fail],
                                                      val async: Async[M],
                                                      val failer: Failer[Fail],
                                                      val cacheFactory: CacheFactory[M],
                                                      val timeService: NanoTimeService,
                                                      val logReqAndResult: LogRequestAndResult[Fail],
                                                      val putMetrics: PutMetrics,
                                                      val httpFactory: HttpFactory[M, ServiceRequest, ServiceResponse],
                                                      val detailedLoggingForSR: DetailedLogging[ServiceResponse])
  extends MicroserviceBuilder[M, Fail] with MicroserviceComposers[M] with EnrichLanguage[M] with MergeLanguage[M] with AnyLanguage with MonadLanguage with AsyncLanguage {

  val keepalive: ServiceRequest => M[Option[ServiceResponse]] = sr => Option(ServiceResponse("Alive")).liftM

  var endpoints: ServiceRequest => M[Option[ServiceResponse]] = chain(keepalive)


}

object Website extends App {
  implicit val ssl: Option[SSLContext] = None

  private val domain: Domain = Domain(Protocol("http"), HostName("localhost"), Port(9000))
  implicit val httpFactory = new HttpFactory[IdentityMonad, ServiceRequest, ServiceResponse] {
    override def apply(v1: ServiceName) = HttpClient.apply[IdentityMonad](domain)
  }
  implicit val loggingAdapter = PrintlnLoggingAdapter
  implicit val resourceBundle = ResourceBundle.getBundle("messages")
  implicit val putMetrics = PrintlnPutMetrics
  implicit val logRequestAndResult: LogRequestAndResult[Throwable] = new SimpleLogRequestAndResult
  implicit val cacheFactory = new CachingServiceFactory[IdentityMonad](DurationStaleCacheStategy(10000000000L, 10000000000000L), NoMapSizeStrategy)

  implicit val executors = Executors.newFixedThreadPool(10)

  import one.xingyi.core.http.Failer.failerForThrowable


  val website = new Website[IdentityMonad, Throwable, JValue]
  val server = new SimpleHttpServer(9000, new EndpointHandler[IdentityMonad, Throwable](website.endpoints))

  println("running")
  server.start()
}
