package one.xingyi.sampleServer

import java.util.ResourceBundle
import java.util.concurrent.Executors

import one.xingyi.core.cache.{CachingServiceFactory, DurationStaleCacheStategy}
import one.xingyi.core.functions.AsyncForScalaFuture._
import one.xingyi.core.functions.MonadCanFail
import one.xingyi.core.http._
import one.xingyi.core.local.ExecutionContextWithLocal
import one.xingyi.core.logging.{AbstractLogRequestAndResult, LogRequestAndResult, PrintlnLoggingAdapter}
import one.xingyi.core.map.NoMapSizeStrategy
import one.xingyi.core.metrics.PrintlnPutMetrics
import one.xingyi.core.simpleServer.{EndpointHandler, SimpleHttpServer}
import one.xingyi.core.strings.IndentAnd
import one.xingyi.sample.{BillboardSetup, FnordSetup, VogueSetup}
import one.xingyi.tagless.{TaglessInterpreterForToString, TaglessLanguageLanguageForKleislis, _}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds

class AllProducers[M[_], Wrapper[_, _], Fail](language: TaglessLanguage[M, Wrapper])(implicit
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

object AllProducers extends App {
  println("All producers")
  implicit val executors = Executors.newFixedThreadPool(10)
  implicit val exc = new ExecutionContextWithLocal(ExecutionContext.fromExecutor(executors))

  implicit val httpFactory = new HttpFactory[Future, ServiceRequest, ServiceResponse] {
    override def apply(v1: ServiceName) = { req => Future.successful(ServiceResponse(Status(200), Body(s"response; ${req.body.map(_.s).getOrElse("")}"), ContentType("text/html"))) }
  }
  implicit val loggingAdapter = PrintlnLoggingAdapter
  implicit val resourceBundle = ResourceBundle.getBundle("messages")
  implicit val putMetrics = PrintlnPutMetrics
  implicit val logRequestAndResult: LogRequestAndResult[Throwable] = new AbstractLogRequestAndResult[Throwable] {
    override protected def format(messagePrefix: String, messagePostFix: String)(strings: String*) = messagePostFix + "." + messagePostFix + ":" + strings.mkString(",")
  }
  //  implicit val cacheFactory = CaffeineCache.cacheFactoryForFuture(CaffeineCache.defaultCacheBuilder)
  implicit val cacheFactory = new CachingServiceFactory[Future](DurationStaleCacheStategy(10000000000L, 10000000000000L), NoMapSizeStrategy)

  import one.xingyi.core.http.Failer.failerForThrowable

  //TODO It would be nice to make this nicer!

  def microservice: ServiceRequest => Future[Option[ServiceResponse]] = {
    val interpreter = new TaglessLanguageLanguageForKleislis[Future, Throwable]
    val kleisliLanguage: interpreter.NonFunctionalLanguageService = interpreter.NonFunctionalLanguageService()

    val experiment = new TaglessModelLanguage[Future] {}
    val modelLanguage = new experiment.ModelLanguage(kleisliLanguage)
    val model: AllProducers[Future, experiment.Model, Throwable] = new AllProducers(modelLanguage)
    val x: experiment.Model[ServiceRequest, Option[ServiceResponse]] = model.rawMicroservice.map(new experiment.ProfileTx("/profile")).map(new experiment.StructureTx("/structure"))
    //still have the problem of how to add the endpoints, although this feels pretty clean...
    val k = x.kleisli

    val profile2 = new Profile2[Future]
    val profiledAllLanguage = profile2.Language(kleisliLanguage)

    val htmlEndpoint = TaglessInterpreterForToString.systemHtmlEndpoint("/html", profiledAllLanguage)(new AllProducers(_).allEndpoints())

    val (allProducers, profileEndpoint) = profile2.makeSystemAndProfileEndpoint(kleisliLanguage, "/profiles", new AllProducers(_),
      { allProducers: AllProducers[Future, profile2.ProfilingWrapper, _] => allProducers.allEndpoints() })

    val microservice = allProducers.allEndpoints(htmlEndpoint, profileEndpoint)
    val indentAndString = microservice.indents(pf => (s"${pf.name} ${pf.description}", pf.tryProfileData.toShortString))
    println(indentAndString.invertIndent.toString("\n", IndentAnd.tupleToString(" ", 40)))
    microservice
  }

  new SimpleHttpServer(9000, new EndpointHandler[Future, Throwable](microservice)).start()
}
