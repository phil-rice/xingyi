package org.validoc.sampleServer

import java.util.ResourceBundle
import java.util.concurrent.Executors

import org.validoc.sample._
import org.validoc.sample.domain.SampleJsonsForCompilation
import org.validoc.simpleServer.{EndpointHandler, SimpleHttpServer}
import org.validoc.tagless.TaglessInterpreterForToString.StringHolder
import org.validoc.tagless.{TaglessInterpreterForToString, TaglessLanguageLanguageForKleislis, _}
import org.validoc.utils.cache.{CachingServiceFactory, DurationStaleCacheStategy}
import org.validoc.utils.functions.AsyncForScalaFuture._
import org.validoc.utils.functions.MonadCanFail
import org.validoc.utils.http._
import org.validoc.utils.local.ExecutionContextWithLocal
import org.validoc.utils.logging.{AbstractLogRequestAndResult, LogRequestAndResult, PrintlnLoggingAdapter}
import org.validoc.utils.map.NoMapSizeStrategy
import org.validoc.utils.metrics.PrintlnPutMetrics
import org.validoc.utils.profiling.{DontProfile, ProfileAs, ProfileAsFail, ProfileAsSuccess}
import org.validoc.utils.strings.IndentAnd

import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

class AllProducers[ M[_],Wrapper[_, _],  Fail](language: TaglessLanguage[M, Wrapper])(implicit
                                                                                     monadCanFail: MonadCanFail[M, Fail],
                                                                                     failer: Failer[Fail],
                                                                                     jsonBundle: JsonBundle) {
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

  import org.validoc.utils.http.Failer.failerForThrowable

  implicit val jsonBundle = JsonBundle()

  //TODO It would be nice to make this nicer!

  def microservice: ServiceRequest => Future[Option[ServiceResponse]] = {
    val interpreter = new TaglessLanguageLanguageForKleislis[Future, Throwable]
    val kleisliLanguage: interpreter.NonFunctionalLanguageService = interpreter.NonFunctionalLanguageService()
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
