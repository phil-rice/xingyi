package org.validoc.sampleServer

import java.util.ResourceBundle
import java.util.concurrent.Executors

import org.validoc.sample.domain.SampleJsonsForCompilation
import org.validoc.sample.{JsonBundle, PromotionSetup}
import org.validoc.utils.simpleServer.{EndpointHandler, SimpleHttpServer}
import org.validoc.tagless.TaglessLanguageLanguageForKleislis
import org.validoc.utils.cache.{CachingServiceFactory, DurationStaleCacheStategy}
import org.validoc.utils.functions.AsyncForScalaFuture.ImplicitsForTest._
import org.validoc.utils.functions.AsyncForScalaFuture._
import org.validoc.utils.http._
import org.validoc.utils.logging.{AbstractLogRequestAndResult, LogRequestAndResult, PrintlnLoggingAdapter}
import org.validoc.utils.map.NoMapSizeStrategy
import org.validoc.utils.metrics.PrintlnPutMetrics

import scala.concurrent.Future

object SampleServer extends App with SampleJsonsForCompilation {

  implicit val httpFactory = new HttpFactory[Future, ServiceRequest, ServiceResponse] {
    override def apply(v1: ServiceName) = { req => Future.successful(ServiceResponse(Status(200), Body(s"response; ${req.body.map(_.s).getOrElse("")}"), ContentType("text/html"))) }
  }
  implicit val loggingAdapter = PrintlnLoggingAdapter
  implicit val resourceBundle = ResourceBundle.getBundle("messages")
  implicit val putMetrics = PrintlnPutMetrics
  implicit val logRequestAndResult: LogRequestAndResult[Throwable] = new AbstractLogRequestAndResult[Throwable] {
    override protected def format(messagePrefix: String, messagePostFix: String)(strings: String*) = messagePostFix + "." + messagePostFix + ":" + strings.mkString(",")
  }
  implicit val cacheFactory = new CachingServiceFactory[Future](DurationStaleCacheStategy(10000000000L, 10000000000000L), NoMapSizeStrategy)

  val interpreter = new TaglessLanguageLanguageForKleislis[Future, Throwable]

  implicit val jsonBundle: JsonBundle = JsonBundle()

  implicit val executors = Executors.newFixedThreadPool(10)

  import org.validoc.utils.http.Failer.failerForThrowable

  private val language = interpreter.NonFunctionalLanguageService()
  //  private val debugLanguage = new DebugEachObjectifyEndpoint(language)
  val setup = new PromotionSetup[Future, interpreter.Kleisli,  Throwable](language)

  //  println("Dumping")
  //  println(debugLanguage.dump)
  new SimpleHttpServer(9000, new EndpointHandler[Future, Throwable](setup.microservice)).start()
}
