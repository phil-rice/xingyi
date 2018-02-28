package org.validoc.sampleServer

import java.util.ResourceBundle
import java.util.concurrent.Executors

import org.validoc.caffeine.CaffeineCache
import org.validoc.sample.PromotionSetup
import org.validoc.simpleServer.{EndpointHandler, SimpleHttpServer}
import org.validoc.utils.concurrency.AsyncForScalaFuture.ImplicitsForTest._
import org.validoc.utils.concurrency.AsyncForScalaFuture._
import org.validoc.utils.http._
import org.validoc.utils.logging.{LogRequestAndResult, PrintlnLoggingAdapter}
import org.validoc.utils.metrics.PrintlnPutMetrics
import org.validoc.utils.tagless.{HttpFactory, TaglessLanguage, TaglessLanguageLanguageForKleislis}

import scala.concurrent.Future

object SampleServer extends App {
  val w = implicitly[Failer[Throwable]]
  implicit val httpFactory = new HttpFactory[Future, ServiceRequest, ServiceResponse] {
    override def apply(v1: ServiceName) = { req => Future.successful(ServiceResponse(Status(200), Body(s"response; ${req.body.map(_.s).getOrElse("")}"), ContentType("text/html"))) }
  }
  implicit val loggingAdapter = PrintlnLoggingAdapter
  implicit val resourceBundle = ResourceBundle.getBundle("messages")
  implicit val putMetrics = PrintlnPutMetrics
  implicit val logRequestAndResult: LogRequestAndResult[Throwable] = new LogRequestAndResult[Throwable]
  implicit val cacheFactory = CaffeineCache.cacheFactoryForFuture(CaffeineCache.defaultCacheBuilder)
  val interpreter = new TaglessLanguageLanguageForKleislis[Future, Throwable, ServiceRequest, ServiceResponse]
  //  type K = interpreter.K
  //  type KOpt[Req, Res] = interpreter.KOpt[Req, Res]
  //
  //implicit  val x: TaglessLanguage[KOpt, K, Throwable, ServiceRequest, ServiceResponse] = interpreter.NonFunctionalLanguageService

  import PromotionSetup.jsonBundle


  implicit val executors = Executors.newFixedThreadPool(10)

  val setup = new PromotionSetup[interpreter.EndpointK, interpreter.Kleisli, Throwable, ServiceRequest, ServiceResponse](interpreter.NonFunctionalLanguageService)

  new SimpleHttpServer(9000, new EndpointHandler[Future, Throwable](setup.microservice))
}
