package org.validoc.sampleServer

import java.util.ResourceBundle
import java.util.concurrent.Executors

import org.validoc.sample._
import org.validoc.sample.domain.SampleJsonsForCompilation
import org.validoc.simpleServer.{EndpointHandler, SimpleHttpServer}
import org.validoc.tagless.{TaglessInterpreterForToString, TaglessLanguageLanguageForKleislis, _}
import org.validoc.utils.cache.{CachingServiceFactory, DurationStaleCacheStategy}
import org.validoc.utils.functions.AsyncForScalaFuture._
import org.validoc.utils.functions.MonadCanFail
import org.validoc.utils.http._
import org.validoc.utils.local.ExecutionContextWithLocal
import org.validoc.utils.logging.{AbstractLogRequestAndResult, LogRequestAndResult, PrintlnLoggingAdapter}
import org.validoc.utils.map.NoMapSizeStrategy
import org.validoc.utils.metrics.PrintlnPutMetrics

import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds

class AllProducers[EndpointWrapper[_, _], Wrapper[_, _], M[_], Fail](language: TaglessLanguage[EndpointWrapper, Wrapper, M, Fail])(implicit
                                                                                                                                   monadCanFail: MonadCanFail[M, Fail],
                                                                                                                                   failer: Failer[M, Fail],
                                                                                                                                   jsonBundle: JsonBundle
) {
  val vogueSetup = new VogueSetup(language)
  val billboardSetup = new BillboardSetup(language)
  val fnordSetup = new FnordSetup(language)
  val endpoints = Seq[EndpointWrapper[_, _]](
    vogueSetup.mostpopularEndpoint,
    billboardSetup.billboardEndpoint,
    fnordSetup.productionEndpoint,
    fnordSetup.programmeEndpoint)
  val rawMicroservice = language.chain(endpoints: _*)
  def allEndpoints(otherEndpoints: EndpointWrapper[_, _]*): Wrapper[ServiceRequest, ServiceResponse] = language.chain((endpoints ++ otherEndpoints): _*)
}

object AllProducers extends App with SampleJsonsForCompilation {
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

  implicit val jsonBundle = JsonBundle()

  //TODO It would be nice to make this nicer!
  //The types are horrific!
  //This is where we are turning the language description into useful endpoints. So It's pretty cool.
  private val forTostring = new TaglessInterpreterForToString()
  val interpreter = new TaglessLanguageLanguageForKleislis[Future, Throwable]
  val kleisliLanguage: interpreter.NonFunctionalLanguageService = interpreter.NonFunctionalLanguageService()
  val language = new ProfileEachEndpointLanguage(kleisliLanguage)
  //  val language = new DebugEachObjectifyEndpoint(profileLanguage)
  val htmlEndpoint = forTostring.systemHtmlEndpoint[interpreter.EndpointK, interpreter.Kleisli, Future, Throwable]("/html", language)(language => new AllProducers[forTostring.StringHolder, forTostring.StringHolder, Future, Throwable](language).allEndpoints())
  private val microservice: interpreter.Kleisli[ServiceRequest, ServiceResponse] = new AllProducers(language).allEndpoints(htmlEndpoint, language.profileMetricsEndpoint)


  new SimpleHttpServer(9000, new EndpointHandler[Future, Throwable](microservice)).start()
}
