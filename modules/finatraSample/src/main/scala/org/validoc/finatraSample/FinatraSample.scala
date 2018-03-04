package org.validoc.finatraSample

import java.util.ResourceBundle
import java.util.concurrent.Executors

import com.twitter.finagle.Http
import com.twitter.finagle.http.{Request, Response}
import com.twitter.finatra.http.Controller
import com.twitter.finatra.utils.FuturePools
import com.twitter.util.{Future, FuturePool}
import org.validoc.finatra._
import org.validoc.sample.domain._
import org.validoc.sample.{JsonBundle, PromotionServiceNames, PromotionSetup}
import org.validoc.utils.cache.{Cachable, CacheFactory}
import org.validoc.utils.http._
import org.validoc.utils.logging.{AbstractLogRequestAndResult, LogRequestAndResult, PrintlnLoggingAdapter}
import org.validoc.utils.metrics.PrintlnPutMetrics
import org.validoc.utils.tagless.{HttpFactory, TaglessLanguageLanguageForKleislis}


class FinatraPromotionSetup(implicit  futurePool: FuturePool) extends Controller with SampleJsonsForCompilation {
  implicit val monad = new AsyncForTwitterFuture
  implicit val httpFactory = new HttpFactory[Future, ServiceRequest, ServiceResponse] {
    override def apply(v1: ServiceName) = { req => Future.value(ServiceResponse(Status(200), Body(s"response; ${req.body.map(_.s).getOrElse("")}"), ContentType("text/html"))) }
  }
  implicit val loggingAdapter = PrintlnLoggingAdapter
//  implicit val resourceBundle = ResourceBundle.getBundle("messages")
  implicit val putMetrics = PrintlnPutMetrics
  implicit val logRequestAndResult: LogRequestAndResult[Throwable] = new AbstractLogRequestAndResult[Throwable] {
    override protected def format(messagePrefix: String, messagePostFix: String)(strings: String*) = messagePostFix + "." + messagePostFix + ":" + strings.mkString(",")
  }
  implicit val cacheFactory = new CacheFactory[Future] {
//    override def apply[Req: Cachable, Res](name: String, raw: Req => Future[Res]): Req => Future[Res] = raw
    override def apply[Req: Cachable, Res](name: String, raw: Req => Future[Res]) = ???

  }

  val interpreter = new TaglessLanguageLanguageForKleislis[Future, Throwable]

  implicit val jsonBundle: JsonBundle = JsonBundle()
  implicit val executors = Executors.newFixedThreadPool(10)
  private val language = interpreter.NonFunctionalLanguageService()
  //  private val debugLanguage = new DebugEachObjectifyEndpoint(language)
  val setup = new PromotionSetup[interpreter.EndpointK, interpreter.Kleisli, Future, Throwable](language)

  get("/") {
    request: Request =>
      //      val serviceRequest = ???
      //      setup.homePageEndPoint(serviceRequest)
      response.ok("some test")
  }

}

object FinatraSample extends App with PromotionServiceNames {
  val setup = new FinatraPromotionSetup()(FuturePools.fixedPool("FuturePoolForApp", 20))

  //  val setup = new PromotionSetup[ServiceData, Future, Request, Response](new AggregatedServicesInterpreter)
  //  val sd = setup.homePageService


  new FinatraServer(8080, new PingController, setup).main(args)

}
