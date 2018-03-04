package org.validoc.sampleServer

import java.util.ResourceBundle
import java.util.concurrent.Executors

import org.validoc.caffeine.CaffeineCache
import org.validoc.sample._
import org.validoc.sample.domain.SampleJsonsForCompilation
import org.validoc.simpleServer.{EndpointHandler, SimpleHttpServer}
import org.validoc.utils.http._
import org.validoc.utils.logging.{AbstractLogRequestAndResult, LogRequestAndResult, PrintlnLoggingAdapter}
import org.validoc.utils.metrics.PrintlnPutMetrics
import org.validoc.utils.tagless.{HttpFactory, TaglessLanguageLanguageForKleislis}

import scala.concurrent.{ExecutionContext, Future}
import org.validoc.utils.functions.AsyncForScalaFuture._
import org.validoc.utils.local.ExecutionContextWithLocal

object AllProducers extends App with SampleJsonsForCompilation {
println("All producers")
  implicit val executors = Executors.newFixedThreadPool(10)
  implicit val exc = ExecutionContextWithLocal.default(ExecutionContext.fromExecutor(executors))

  implicit val httpFactory = new HttpFactory[Future, ServiceRequest, ServiceResponse] {
    override def apply(v1: ServiceName) = { req => Future.successful(ServiceResponse(Status(200), Body(s"response; ${req.body.map(_.s).getOrElse("")}"), ContentType("text/html"))) }
  }
  implicit val loggingAdapter = PrintlnLoggingAdapter
  implicit val resourceBundle = ResourceBundle.getBundle("messages")
  implicit val putMetrics = PrintlnPutMetrics
  implicit val logRequestAndResult: LogRequestAndResult[Throwable] = new AbstractLogRequestAndResult[Throwable] {
    override protected def format(messagePrefix: String, messagePostFix: String)(strings: String*) = messagePostFix + "." + messagePostFix + ":" + strings.mkString(",")
  }
  implicit val cacheFactory = CaffeineCache.cacheFactoryForFuture(CaffeineCache.defaultCacheBuilder)

  val interpreter = new TaglessLanguageLanguageForKleislis[Future, Throwable]

  implicit val jsonBundle = JsonBundle()
  private val language = interpreter.NonFunctionalLanguageService()
  val vogueSetup = new VogueSetup(language)
  val billboardSetup = new BillboardSetup(language)
  val fnordSetup = new FnordSetup(language)

  val microService = language.chain(vogueSetup.mostpopularEndpoint, billboardSetup.billboardEndpoint, fnordSetup.productionEndpoint, fnordSetup.programmeEndpoint)


  new SimpleHttpServer(9000, new EndpointHandler[Future, Throwable](microService)).start()
}
