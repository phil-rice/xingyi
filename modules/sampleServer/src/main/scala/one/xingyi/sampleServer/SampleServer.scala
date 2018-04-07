package one.xingyi.sampleServer

import java.util.ResourceBundle
import java.util.concurrent.Executors

import one.xingyi.sample.{JsonBundle, PromotionSetup}
import one.xingyi.sample.domain.SampleJsonsForCompilation
import one.xingyi.tagless.TaglessLanguageLanguageForKleislis
import one.xingyi.utils.cache.{CachingServiceFactory, DurationStaleCacheStategy}
import one.xingyi.utils.functions.AsyncForScalaFuture.ImplicitsForTest._
import one.xingyi.utils.functions.AsyncForScalaFuture._
import one.xingyi.utils.http._
import one.xingyi.utils.logging.{AbstractLogRequestAndResult, LogRequestAndResult, PrintlnLoggingAdapter}
import one.xingyi.utils.map.NoMapSizeStrategy
import one.xingyi.utils.metrics.PrintlnPutMetrics
import one.xingyi.utils.simpleServer.{EndpointHandler, SimpleHttpServer}

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

  import one.xingyi.utils.http.Failer.failerForThrowable

  private val language = interpreter.NonFunctionalLanguageService()
  //  private val debugLanguage = new DebugEachObjectifyEndpoint(language)
  val setup = new PromotionSetup[Future, interpreter.Kleisli,  Throwable](language)

  //  println("Dumping")
  //  println(debugLanguage.dump)
  new SimpleHttpServer(9000, new EndpointHandler[Future, Throwable](setup.microservice)).start()
}
