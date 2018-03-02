package org.validoc.utils.tagless

import java.util.concurrent.TimeUnit

import org.validoc.utils.UtilsSpec
import org.validoc.utils.endpoint.IdAtEndAndVerb
import org.validoc.utils.http._
import org.validoc.utils.profiling.TryProfileData
import org.validoc.utils.retry.RetryConfig
import org.validoc.utils.strings.Strings
import org.validoc.utils.success.MessageName
import org.validoc.utils.time.RandomDelay

import scala.concurrent.duration.FiniteDuration
import scala.language.{higherKinds, implicitConversions}
import org.validoc.utils.endpoint.MatchesServiceRequest._

class TaglessSpec extends UtilsSpec with HttpObjectFixture {


  class Sample[EndpointWrapper[_, _], Wrapper[_, _], Fail](implicit httpLanguage: TaglessLanguage[EndpointWrapper, Wrapper, Fail, HttpReq, HttpRes], failer: Failer[Fail]) {

    import httpLanguage._

    implicit def fn(s: String) = s;

    implicit val messageName = MessageName[String, String]("someMessageName")

    implicit object ParserForStringString extends ResponseParser[Fail, String, String] {
      override def apply(req: String) = { res => Right(req + "=>" + res) }
    }

    implicit object EnricherForStrings extends Enricher[String, String, String, String, String] {
      override def apply(v1: String, v2: String, v3: Seq[(String, String)]) = List(v1, v2, v3).mkString(",")
    }

    implicit object HasChildrenForString extends HasChildren[String, String] {
      override def apply(v1: String) = v1.split(",")
    }

    implicit object FindReqForString extends FindReq[String, String] {
      override def apply(v1: String) = v1
    }

    implicit object FromServiceRequestForString extends FromServiceRequest[String] {
      override def apply(v1: ServiceRequest): String = Strings.lastSection("/")(v1.body.map(_.s).getOrElse(throw new RuntimeException))
    }

    implicit object ToServiceResponseForString extends ToServiceResponse[String] {
      override def apply(v1: String): ServiceResponse = ServiceResponse(Status(200), Body(v1), ContentType("text/plain"))
    }

    implicit def toSName(s: String) = ServiceName(s)


    val retryConfig = RetryConfig(10, RandomDelay(FiniteDuration(100, TimeUnit.MILLISECONDS)))


    def s1: Wrapper[String, String] = http("s1") |+| objectify[String, String] |+| logging("service1") |+| metrics("service1")

    val data = new TryProfileData

    def s2 = http("s2") |+| objectify[String, String] |+| logging("service1") |+| profile(data)
    def s3 = http("s3") |+| objectify[String, String]
    def s4 = http("s4") |+| objectify[String, String] |+| retry(retryConfig)

    val e1: Wrapper[String, String] = enrich(s1).withChild(s2).mergeInto[String]

    def m2 = merge(s1) and s2 into ((_: String, _: String, _: String) => "someFn")
    def m3 = merge(s1) and s2 and s3 into ((_: String, _: String, _: String, _: String) => "someFn")
    def m4 = merge(s1) and s2 and s3 and s4 into ((_: String, _: String, _: String, _: String, _: String) => "someFn")
    //    def m3 = merge3(s1, s2, s3, (_: String, _: String, _: String, _: String) => "someFn")
    //    def m4 = merge4(s1, s2, s3, s4, (_: String, _: String, _: String, _: String, _: String) => "someFn")

    def endpoint1: EndpointWrapper[String, String] = endpoint("/endpoint1", idAtEnd(Get))(e1)
    def endpoint2: EndpointWrapper[String, String] = endpoint("/endpoint2", idAtEnd(Get))(s1)

    def microservice: Wrapper[ServiceRequest, ServiceResponse] = chain(endpoint1, endpoint2)

//    def endPointx: Wrapper[String, String] => EndpointWrapper[String, String] = endpoint[String, String]("/endpoint2", idAtEnd(Get)) _
//    def endPointy = endpoint[String, String]("/endpoint2", idAtEnd(Get)) _ <++> objectify[String, String] _
//    def endPointz = endpoint[String, String]("/endpoint2", idAtEnd(Get)) _ <++> objectify[String,String] _ <++> http("s6")
//
//
//    def endPointw: EndpointWrapper[String, String] = endpoint[String,String]("/endpoint2", idAtEnd(Get)) _ <++> objectify _ <+> http("s6")
    //

    //    def endPointz: EndpointWrapper[String, String] = endpoint[String, String]("/endpoint2", idAtEnd(Get)) _ <+> objectify[String,String] <++> http("sa1")
    //    def endPointe = endpoint[String, String]("/endpoint2", idAtEnd(Get)) _ <+> objectify <++> http("sa1")

  }

  behavior of "Tagless with toString Interpreter"

  it should "handle" in {
    implicit val stringlanguage = new TaglessInterpreterForToString[HttpReq, HttpRes]
    import stringlanguage._


    val sample = new Sample[StringHolder, StringHolder, Void]()
    sample.s1.lines shouldBe List((3, "metrics(service1)"), (2, "logging(service1 using prefix someMessageName)"), (1, "objectify[String,String]"), (0, "http(s1)"))

    sample.m2.lines shouldBe List(
      (4, "merge2"),
      (3, "metrics(service1)"), (2, "logging(service1 using prefix someMessageName)"), (1, "objectify[String,String]"), (0, "http(s1)"),
      (3, "profile"), (2, "logging(service1 using prefix someMessageName)"), (1, "objectify[String,String]"), (0, "http(s2)"))
    sample.m3.lines shouldBe List(
      (4, "merge3"),
      (3, "metrics(service1)"), (2, "logging(service1 using prefix someMessageName)"), (1, "objectify[String,String]"), (0, "http(s1)"),
      (3, "profile"), (2, "logging(service1 using prefix someMessageName)"), (1, "objectify[String,String]"), (0, "http(s2)"),
      (3, "objectify[String,String]"), (2, "http(s3)"))

    //      :List((4,merge4), (3,metrics(service1)), (2,logging(service1 using prefix someMessageName)), (1,objectify[String,String]), (0,http(s1)), (2,logging(service1 using prefix someMessageName)), (1,objectify[String,String]), (0,http(s2)), (1,objectify[String,String]), (0,http(s3)), (1,objectify[String,String]), (0,http(s4)))

    sample.m4.lines shouldBe List(
      (4, "merge4"),
      (3, "metrics(service1)"), (2, "logging(service1 using prefix someMessageName)"), (1, "objectify[String,String]"), (0, "http(s1)"),
      (3, "profile"), (2, "logging(service1 using prefix someMessageName)"), (1, "objectify[String,String]"), (0, "http(s2)"),
      (3, "objectify[String,String]"), (2, "http(s3)"),
      (3, "retry(RetryConfig(10,RandomDelay(100 milliseconds)))"), (2, "objectify[String,String]"), (1, "http(s4)"))

    sample.e1.lines shouldBe List(
      (4, "enrich"),
      (3, "metrics(service1)"), (2, "logging(service1 using prefix someMessageName)"), (1, "objectify[String,String]"), (0, "http(s1)"),
      (3, "profile"), (2, "logging(service1 using prefix someMessageName)"), (1, "objectify[String,String]"), (0, "http(s2)"))
    sample.endpoint1.lines shouldBe List(
      (5, "endpoint[String,String](/endpoint1,IdAtEndAndVerb(Get))"),
      (4, "enrich"),
      (3, "metrics(service1)"), (2, "logging(service1 using prefix someMessageName)"), (1, "objectify[String,String]"), (0, "http(s1)"),
      (3, "profile"), (2, "logging(service1 using prefix someMessageName)"), (1, "objectify[String,String]"), (0, "http(s2)"))

    sample.endpoint2.lines shouldBe List((4, "endpoint[String,String](/endpoint2,IdAtEndAndVerb(Get))"), (3, "metrics(service1)"), (2, "logging(service1 using prefix someMessageName)"), (1, "objectify[String,String]"), (0, "http(s1)"))

    sample.microservice.lines shouldBe List(
      (6, "chain"),
      (5, "endpoint[String,String](/endpoint1,IdAtEndAndVerb(Get))"),
      (4, "enrich"),
      (3, "metrics(service1)"), (2, "logging(service1 using prefix someMessageName)"), (1, "objectify[String,String]"), (0, "http(s1)"),
      (3, "profile"), (2, "logging(service1 using prefix someMessageName)"), (1, "objectify[String,String]"), (0, "http(s2)"),
      (5, "endpoint[String,String](/endpoint2,IdAtEndAndVerb(Get))"), (4, "metrics(service1)"), (3, "logging(service1 using prefix someMessageName)"), (2, "objectify[String,String]"), (1, "http(s1)")
    )

  }

}
