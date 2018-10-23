/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.sampleServer

import java.util.ResourceBundle
import java.util.concurrent.Executors

import one.xingyi.core.cache.{CachingServiceFactory, DurationStaleCacheStategy}
import one.xingyi.core.http._
import one.xingyi.core.json.{JsonParser, JsonWriter}
import one.xingyi.core.local.ExecutionContextWithLocal
import one.xingyi.core.logging.{LogRequestAndResult, PrintlnLoggingAdapter, SimpleLogRequestAndResult}
import one.xingyi.core.map.NoMapSizeStrategy
import one.xingyi.core.metrics.PrintlnPutMetrics
import one.xingyi.core.monad.AsyncForScalaFuture._
import one.xingyi.core.monad.MonadCanFail
import one.xingyi.core.simpleServer.{EndpointHandler, SimpleHttpServer}
import one.xingyi.core.strings.IndentAnd
import one.xingyi.sample.{BillboardSetup, FnordSetup, VogueSetup}
import one.xingyi.tagless.{TaglessInterpreterForToString, TaglessLanguageLanguageForKleislis, _}
import org.json4s.JsonAST.JValue

import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds

class AllProducers[M[_], J: JsonWriter:JsonParser, Wrapper[_, _], Fail](language: TaglessLanguage[M, Wrapper])(implicit
                                                                                                               monadCanFail: MonadCanFail[M, Fail],
                                                                                                               failer: Failer[Fail]) {
  val vogueSetup = new VogueSetup(language)
  val billboardSetup = new BillboardSetup(language)
  val fnordSetup = new FnordSetup(language)

  val endpoints: Seq[Wrapper[ServiceRequest, Option[ServiceResponse]]] = Seq(
    vogueSetup.mostpopularEndpoint,
    billboardSetup.billboardEndpoint,
    fnordSetup.productionEndpoint,
    fnordSetup.programmeEndpoint)
  val rawMicroservice = language.chain(endpoints: _*)
  def allEndpoints(otherEndpoints: Wrapper[ServiceRequest, Option[ServiceResponse]]*): Wrapper[ServiceRequest, Option[ServiceResponse]] =
    if (otherEndpoints.size == 0) language.chain(endpoints: _*) else
      language.chain(language.chain(endpoints: _*), language.chain(otherEndpoints: _*))
}

class AllProducersApp(port: Int) {
  import one.xingyi.json4s.Json4sWriter._
  import one.xingyi.json4s.Json4sParser._
  println("All producers")
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

  //TODO It would be nice to make this nicer! Frankly the code sucks even the effect is nice...

  def microservice: ServiceRequest => Future[Option[ServiceResponse]] = {
    val interpreter = new TaglessLanguageLanguageForKleislis[Future, Throwable]
    val kleisliLanguage: interpreter.NonFunctionalLanguageService = interpreter.NonFunctionalLanguageService()

    val experiment = new TaglessModelLanguage[Future] {}
    val modelLanguage = new experiment.ModelLanguage(kleisliLanguage)
    val model: AllProducers[Future, JValue, experiment.Model, Throwable] = new AllProducers(modelLanguage)
    val x: experiment.Model[ServiceRequest, Option[ServiceResponse]] = model.rawMicroservice.map(new experiment.ProfileTx("/profile")).map(new experiment.StructureTx("/structure"))
    //still have the problem of how to add the endpoints, although this feels pretty clean...
    val k = x.kleisli

    val profile2 = new Profile2[Future]
    val profiledAllLanguage = profile2.Language(kleisliLanguage)

    val htmlEndpoint = TaglessInterpreterForToString.systemHtmlEndpoint("/html", profiledAllLanguage)(new AllProducers(_).allEndpoints())

    val (allProducers, profileEndpoint) = profile2.makeSystemAndProfileEndpoint(kleisliLanguage, "/profiles", new AllProducers(_),
      { allProducers: AllProducers[Future, JValue, profile2.ProfilingWrapper, _] => allProducers.allEndpoints() })

    val microservice = allProducers.allEndpoints(htmlEndpoint, profileEndpoint)
    val indentAndString = microservice.indents(pf => (s"${pf.name} ${pf.description}", pf.tryProfileData.toShortString))
    println(indentAndString.invertIndent.toString("\n", IndentAnd.tupleToString(" ", 40)))
    microservice
  }

  val server = new SimpleHttpServer(port, new EndpointHandler[Future, Throwable](microservice))
}

object AllProducersApp extends AllProducersApp(port = 9010) {
  server.start()
}