package one.xingyi.finatraSample

import java.util.concurrent.Executors

import com.twitter.finagle.http.Request
import com.twitter.finatra.http.Controller
import com.twitter.finatra.utils.FuturePools
import com.twitter.util.{Future, FuturePool}
import one.xingyi.finatra.{AsyncForTwitterFuture, FinatraServer, PingController}
import one.xingyi.sample.domain.SampleJsonsForCompilation
import one.xingyi.tagless.TaglessLanguageLanguageForKleislis
import one.xingyi.sample.domain._
import one.xingyi.sample.{JsonBundle, PromotionServiceNames, PromotionSetup}
import one.xingyi.utils.cache._
import one.xingyi.utils.http._
import one.xingyi.utils.logging.{AbstractLogRequestAndResult, LogRequestAndResult, PrintlnLoggingAdapter}
import one.xingyi.utils.map.MaxMapSizeStrategy
import one.xingyi.utils.metrics.PrintlnPutMetrics


class FinatraPromotionSetup(implicit futurePool: FuturePool) extends Controller with SampleJsonsForCompilation {
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
  implicit val cacheFactory = new CachingServiceFactory[Future](new DurationStaleCacheStategy(100000000l, 10000000000l), new MaxMapSizeStrategy(1000, 100))

  val interpreter = new TaglessLanguageLanguageForKleislis[Future, Throwable]

  implicit val jsonBundle: JsonBundle = JsonBundle()
  implicit val executors = Executors.newFixedThreadPool(10)

  import one.xingyi.utils.http.Failer.failerForThrowable

  private val language = interpreter.NonFunctionalLanguageService()
  //  private val debugLanguage = new DebugEachObjectifyEndpoint(language)
  val setup = new PromotionSetup[Future, interpreter.Kleisli, Throwable](language)

  import one.xingyi.finatra.FinatraImplicits._

  def liftEndpoint(fn: ServiceRequest => Future[Option[ServiceResponse]]) = { request: Request =>
    val serviceRequest = implicitly[ToServiceRequest[Request]] apply (request)
    val result = fn(serviceRequest)
    result.map { case Some(serRes) =>
      response.status(serRes.status.code).body(serRes.body.s).contentType(serviceRequest.contentType.map(_.s).getOrElse("text/html"))
    }
  }

  get("/")(liftEndpoint(setup.homePageEndPoint))

}

object FinatraSample extends App with PromotionServiceNames {
  val setup = new FinatraPromotionSetup()(FuturePools.fixedPool("FuturePoolForApp", 20))

  //  val setup = new PromotionSetup[ServiceData, Future, Request, Response](new AggregatedServicesInterpreter)
  //  val sd = setup.homePageService


  new FinatraServer(8080, new PingController, setup).main(args)

}
