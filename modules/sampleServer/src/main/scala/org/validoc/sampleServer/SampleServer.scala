package org.validoc.sampleServer

import java.util.ResourceBundle
import java.util.concurrent.Executors

import org.validoc.caffeine.CaffeineCache
import org.validoc.sample.{JsonBundle, PromotionSetup}
import org.validoc.sample.domain.{MostPopular, MostPopularQuery, SampleJsonsForCompilation}
import org.validoc.simpleServer.{EndpointHandler, SimpleHttpServer}
import org.validoc.utils.functions.AsyncForScalaFuture.ImplicitsForTest._
import org.validoc.utils.functions.AsyncForScalaFuture._
import org.validoc.utils.http._
import org.validoc.utils.json.FromJson
import org.validoc.utils.logging.{LogRequestAndResult, PrintlnLoggingAdapter}
import org.validoc.utils.metrics.PrintlnPutMetrics
import org.validoc.utils.parser.Parser
import org.validoc.utils.tagless.{HttpFactory, TaglessLanguageLanguageForKleislis}

import scala.concurrent.Future

object SampleServer extends App with SampleJsonsForCompilation {

  implicit val httpFactory = new HttpFactory[Future, ServiceRequest, ServiceResponse] {
    override def apply(v1: ServiceName) = { req => Future.successful(ServiceResponse(Status(200), Body(s"response; ${req.body.map(_.s).getOrElse("")}"), ContentType("text/html"))) }
  }
  implicit val loggingAdapter = PrintlnLoggingAdapter
  implicit val resourceBundle = ResourceBundle.getBundle("messages")
  implicit val putMetrics = PrintlnPutMetrics
  implicit val logRequestAndResult: LogRequestAndResult[Throwable] = new LogRequestAndResult[Throwable]
  implicit val cacheFactory = CaffeineCache.cacheFactoryForFuture(CaffeineCache.defaultCacheBuilder)
  val interpreter = new TaglessLanguageLanguageForKleislis[Future, Throwable]
  //  type K = interpreter.K
  //  type KOpt[Req, Res] = interpreter.KOpt[Req, Res]
  //
  //implicit  val x: TaglessLanguage[KOpt, K, Throwabe, ServiceRequest, ServiceResponse] = interpreter.NonFunctionalLanguageService

  import PromotionSetup.jsonBundle

  implicit val jsonBundle: JsonBundle = JsonBundle()

  implicit val executors = Executors.newFixedThreadPool(10)

  //  println(s"toRequest1 ${implicitly[ToServiceRequest[MostPopularQuery]]}")
  //  println(s"toRequest2 ${implicitly[ToServiceRequest[ServiceRequest]]}")
  //  println(s"toHttpReq1 ${implicitly[FromServiceRequest[ServiceRequest]]}")
  //  val x = implicitly[Parser[MostPopular]]
  //  println(s"toHttpReq 2${implicitly[FromServiceResponse[MostPopular]]}")
  //  println(s"toServiceResponse2${implicitly[ToServiceResponse[ServiceResponse]]}")
  //  println(s"categoriser ${implicitly[ResponseCategoriser[ServiceResponse]]}")
  //  println(s"responseProcessor ${implicitly[ResponseProcessor[Throwable, MostPopularQuery, MostPopular]]}")
  //  val failer = implicitly[Failer[Throwable]]
  //  val y = ResponseParser.defaultDirtyParser[Throwable, MostPopularQuery, MostPopular]
  //  val responseParser = implicitly[ResponseParser[Throwable,MostPopularQuery,MostPopular]]
  val setup = new PromotionSetup[interpreter.EndpointK, interpreter.Kleisli, Future, Throwable, ServiceRequest, ServiceResponse](interpreter.NonFunctionalLanguageService())


  new SimpleHttpServer(9000, new EndpointHandler[Future, Throwable](setup.microservice)).start()
}
