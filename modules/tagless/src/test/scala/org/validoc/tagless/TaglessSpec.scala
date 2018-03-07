package org.validoc.tagless

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicReference

import org.validoc.utils.UtilsSpec
import org.validoc.utils.aggregate.{Enricher, FindReq, HasChildren}
import org.validoc.utils.endpoint.MatchesServiceRequest._
import org.validoc.utils.functions.MonadCanFail
import org.validoc.utils.http._
import org.validoc.utils.language.Language._
import org.validoc.utils.parser.Parser
import org.validoc.utils.profiling.TryProfileData
import org.validoc.utils.retry.RetryConfig
import org.validoc.utils.strings.{IndentAndString, Strings}
import org.validoc.utils.time.RandomDelay

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.language.{higherKinds, implicitConversions}

class TaglessSpec extends UtilsSpec with HttpObjectFixture {


  class Sample[Wrapper[_, _], M[_], Fail](implicit monadCanFail: MonadCanFail[M, Fail], httpLanguage: TaglessLanguage[Wrapper, M, Fail], failer: Failer[M, Fail]) {

    import httpLanguage._

    implicit def fn(s: String) = s;


    implicit object ParserForStringString extends Parser[String] {
      override def apply(req: String) = req
    }

    implicit object EnricherForStrings extends Enricher[String, String, String, String, String] {
      override def apply(v1: String, v2: String, v3: Seq[(String, String)]) = List(v1, v2, v3).mkString(",")
    }

    implicit object HasChildrenForString extends HasChildren[String, String] {
      override def apply(v1: String) = v1.split(",")
    }

    implicit object FromServiceRequestForString extends FromServiceRequest[M, String] {
      override def apply(v1: ServiceRequest): M[String] = Strings.lastSection("/")(v1.body.map(_.s).getOrElse(throw new RuntimeException)).liftM
    }

    implicit object ToServiceResponseForString extends ToServiceResponse[String] {
      override def apply(v1: String): ServiceResponse = ServiceResponse(Status(200), Body(v1), ContentType("text/plain"))
    }

    implicit def toSName(s: String) = ServiceName(s)


    val retryConfig = RetryConfig(10, RandomDelay(FiniteDuration(100, TimeUnit.MILLISECONDS)))

    //    implicit object logRequestAndResult extends LogRequestAndResult[M, Fail] {
    //      override def apply[Req: DetailedLogging : SummaryLogging, Res: DetailedLogging : SummaryLogging](sender: Any, messagePrefix: String)(req: Req) = ???
    //    }

    ResponseParser.defaultDirtyParser[M, Fail, String, String]
    def s1: Wrapper[String, String] = http("s1") |+| objectify[String, String] |+| logging("prefix1") |+| metrics("service1")

    val data = new TryProfileData

    def s2 = http("s2") |+| objectify[String, String] |+| logging("prefix2") |+| profile(data)
    def s3 = http("s3") |+| objectify[String, String]
    def s4 = http("s4") |+| objectify[String, String] |+| retry(retryConfig)

    def f1 = function("f1") { x: Int => x.toString } |+| cache("f1 cache")
    val e1: Wrapper[String, String] = enrich(s1).withChild(s2).mergeInto[String]

    def m2 = merge(s1) and s2 into ((_: String, _: String, _: String) => "someFn")
    def m3 = merge(s1) and s2 and s3 into ((_: String, _: String, _: String, _: String) => "someFn")
    def m4 = merge(s1) and s2 and s3 and s4 into ((_: String, _: String, _: String, _: String, _: String) => "someFn")
    //    def m3 = merge3(s1, s2, s3, (_: String, _: String, _: String, _: String) => "someFn")
    //    def m4 = merge4(s1, s2, s3, s4, (_: String, _: String, _: String, _: String, _: String) => "someFn")

    def endpoint1 = endpoint("/endpoint1", idAtEnd(Get))(e1)
    def endpoint2 = endpoint("/endpoint2", idAtEnd(Get))(s1)

    def microservice: Wrapper[ServiceRequest, ServiceResponse] = chain(endpoint1, endpoint2)

  }


  behavior of "Tagless with toString Interpreter"
  implicit val stringlanguage: TaglessInterpreterForToString = new TaglessInterpreterForToString

  import org.validoc.utils.functions.AsyncForScalaFuture._
  import ImplicitsForTest._

  type StringHolder[Req, Res] = IndentAndString

  private val toStringLanguage = new TaglessInterpreterForToString().forToString[Future, Throwable]
  it should "turn a sample language into a string with the 'for to string'" in {
    checkTaglessToString(toStringLanguage)
  }
  it should "be able to turn a system definition into a crude html dump" in {
    val ref = new AtomicReference[IndentAndString]()
    val actual = stringlanguage.systemToString[Future, Throwable] { implicit int =>
      val microservice = new Sample().microservice
      ref.set(microservice)
      microservice
    }
    val expected = ref.get().invertIndent.toString("&nbsp;&nbsp;", "<br />")
    actual shouldBe expected
    actual should startWith("chain<br />&nbsp;&nbsp;endpoint[String,String](/endpoint1,IdAt")
  }

  it should "be able to turn a system definition into an endpoint in some interpreter's world with the html in it" in {
    val endPointInStringLanguageWorld = stringlanguage.systemHtmlEndpoint[StringHolder, Future, Throwable]("/endPoints", stringlanguage.forToString)(implicit langauge => new Sample().microservice)
    endPointInStringLanguageWorld.lines shouldBe List((1, "endpoint[ServiceRequest,ServiceResponse](/endPoints,FixedPathAndVerb(Get))"), (0, "function-html"))

    //TODO we should be able to call this but need helper code to make that happen. Then we can check the actual string is generated.
    //    val actualLanguage = new TaglessLanguageLanguageForKleislis[Future, Throwable]
    //    val endPointInKleisliWorld = stringlanguage.systemHtmlEndpoint[actualLanguage.EndpointK, actualLanguage.Kleisli, Future, Throwable]("/endpoints", actualLanguage.NonFunctionalLanguageService())

  }

  it should "have a delegate interpreter that delegates 'for to string'" in {
    checkTaglessToString(new DelegatesTaglessLanguage[StringHolder, Future, Throwable](toStringLanguage))
  }
  //  behavior of "Tagless with default delegate Interpreter"

  //  it should "delegate" in {
  //    checkTaglessToString(new DelegatesTaglessLanguage(new TaglessInterpreterForToString))
  //
  //  }

  private def checkTaglessToString(implicit stringlanguage: TaglessLanguage[StringHolder, Future, Throwable]) = {
    import stringlanguage._
    val sample = new Sample[StringHolder, Future, Throwable]()
    sample.s1.lines shouldBe List((3, "metrics(service1)"), (2, "logging(Using prefix1)"), (1, "objectify[String,String]"), (0, "http(s1)"))

    sample.m2.lines shouldBe List(
      (4, "merge2"),
      (3, "metrics(service1)"), (2, "logging(Using prefix1)"), (1, "objectify[String,String]"), (0, "http(s1)"),
      (3, "profile"), (2, "logging(Using prefix2)"), (1, "objectify[String,String]"), (0, "http(s2)"))
    sample.m3.lines shouldBe List(
      (4, "merge3"),
      (3, "metrics(service1)"), (2, "logging(Using prefix1)"), (1, "objectify[String,String]"), (0, "http(s1)"),
      (3, "profile"), (2, "logging(Using prefix2)"), (1, "objectify[String,String]"), (0, "http(s2)"),
      (3, "objectify[String,String]"), (2, "http(s3)"))

    //      :List((4,merge4), (3,metrics(service1)), (2,logging(Using prefix)), (1,objectify[String,String]), (0,http(s1)), (2,logging(Using prefix)), (1,objectify[String,String]), (0,http(s2)), (1,objectify[String,String]), (0,http(s3)), (1,objectify[String,String]), (0,http(s4)))

    sample.m4.lines shouldBe List(
      (4, "merge4"),
      (3, "metrics(service1)"), (2, "logging(Using prefix1)"), (1, "objectify[String,String]"), (0, "http(s1)"),
      (3, "profile"), (2, "logging(Using prefix2)"), (1, "objectify[String,String]"), (0, "http(s2)"),
      (3, "objectify[String,String]"), (2, "http(s3)"),
      (3, "retry(RetryConfig(10,RandomDelay(100 milliseconds)))"), (2, "objectify[String,String]"), (1, "http(s4)"))

    sample.e1.lines shouldBe List(
      (4, "enrich"),
      (3, "metrics(service1)"), (2, "logging(Using prefix1)"), (1, "objectify[String,String]"), (0, "http(s1)"),
      (3, "profile"), (2, "logging(Using prefix2)"), (1, "objectify[String,String]"), (0, "http(s2)"))
    sample.endpoint1.lines shouldBe List(
      (5, "endpoint[String,String](/endpoint1,IdAtEndAndVerb(Get))"),
      (4, "enrich"),
      (3, "metrics(service1)"), (2, "logging(Using prefix1)"), (1, "objectify[String,String]"), (0, "http(s1)"),
      (3, "profile"), (2, "logging(Using prefix2)"), (1, "objectify[String,String]"), (0, "http(s2)"))

    sample.endpoint2.lines shouldBe List(
      (4, "endpoint[String,String](/endpoint2,IdAtEndAndVerb(Get))"),
      (3, "metrics(service1)"), (2, "logging(Using prefix1)"), (1, "objectify[String,String]"), (0, "http(s1)"))

    sample.microservice.lines shouldBe List(
      (6, "chain"),
      (5, "endpoint[String,String](/endpoint1,IdAtEndAndVerb(Get))"),
      (4, "enrich"),
      (3, "metrics(service1)"), (2, "logging(Using prefix1)"), (1, "objectify[String,String]"), (0, "http(s1)"),
      (3, "profile"), (2, "logging(Using prefix2)"), (1, "objectify[String,String]"), (0, "http(s2)"),
      (5, "endpoint[String,String](/endpoint2,IdAtEndAndVerb(Get))"), (4, "metrics(service1)"), (3, "logging(Using prefix1)"), (2, "objectify[String,String]"), (1, "http(s1)")
    )

    sample.f1.lines shouldBe List((1, "cache(f1 cache)"), (0, "function-f1"))
  }
}
