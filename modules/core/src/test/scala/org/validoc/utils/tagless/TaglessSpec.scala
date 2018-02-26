package org.validoc.utils.tagless

import java.util.concurrent.TimeUnit

import org.validoc.utils.UtilsSpec
import org.validoc.utils.http._
import org.validoc.utils.retry.RetryConfig
import org.validoc.utils.success.MessageName
import org.validoc.utils.time.RandomDelay

import scala.concurrent.duration.FiniteDuration
import scala.language.{higherKinds, implicitConversions}

class TaglessSpec extends UtilsSpec with HttpObjectFixture {


  class Sample[Wrapper[_, _], Fail](implicit httpLanguage: TaglessLanguage[Wrapper, Fail, HttpReq, HttpRes], failer: Failer[Fail]) {

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

    implicit def toSName(s: String) = ServiceName(s)

    val retryConfig = RetryConfig(10, RandomDelay(FiniteDuration(100, TimeUnit.MILLISECONDS)))

    def s1 = http("s1") |+| objectify[String, String] |+| logging("service1") |+| metrics("service1")
    def s2 = http("s2") |+| objectify[String, String] |+| logging("service1")
    def s3 = http("s3") |+| objectify[String, String]
    def s4 = http("s4") |+| objectify[String, String] |+| retry(retryConfig)

    val e1 = enrich(s1).withChild(s2).mergeInto[String]

    def m2 = merge(s1) and s2 into ((_: String, _: String, _: String) => "someFn")
    def m3 = merge(s1) and s2 and s3 into ((_: String, _: String, _: String, _: String) => "someFn")
    def m4 = merge(s1) and s2 and s3 and s4 into ((_: String, _: String, _: String, _: String, _: String) => "someFn")
    //    def m3 = merge3(s1, s2, s3, (_: String, _: String, _: String, _: String) => "someFn")
    //    def m4 = merge4(s1, s2, s3, s4, (_: String, _: String, _: String, _: String, _: String) => "someFn")
  }

  behavior of "Tagless with toString Interpreter"

  it should "handle" in {
    implicit val stringlanguage = new TaglessInterpreterForToString[HttpReq, HttpRes]
    import stringlanguage._


    val sample = new Sample[StringHolder, Void]()
    sample.s1.lines shouldBe List((3, "metrics(service1)"), (2, "logging(service1 using prefix someMessageName)"), (1, "objectify[String,String]"), (0, "http(s1)"))

    sample.m2.lines shouldBe List(
      (4, "merge2"),
      (3, "metrics(service1)"), (2, "logging(service1 using prefix someMessageName)"), (1, "objectify[String,String]"), (0, "http(s1)"),
      (2, "logging(service1 using prefix someMessageName)"), (1, "objectify[String,String]"), (0, "http(s2)"))
    sample.m3.lines shouldBe List(
      (4, "merge3"),
      (3, "metrics(service1)"), (2, "logging(service1 using prefix someMessageName)"), (1, "objectify[String,String]"), (0, "http(s1)"),
      (2, "logging(service1 using prefix someMessageName)"), (1, "objectify[String,String]"), (0, "http(s2)"),
      (1, "objectify[String,String]"), (0, "http(s3)"))

    //      :List((4,merge4), (3,metrics(service1)), (2,logging(service1 using prefix someMessageName)), (1,objectify[String,String]), (0,http(s1)), (2,logging(service1 using prefix someMessageName)), (1,objectify[String,String]), (0,http(s2)), (1,objectify[String,String]), (0,http(s3)), (1,objectify[String,String]), (0,http(s4)))

    sample.m4.lines shouldBe List(
      (4, "merge4"),
      (3, "metrics(service1)"), (2, "logging(service1 using prefix someMessageName)"), (1, "objectify[String,String]"), (0, "http(s1)"),
      (2, "logging(service1 using prefix someMessageName)"), (1, "objectify[String,String]"), (0, "http(s2)"),
      (1, "objectify[String,String]"), (0, "http(s3)"),
      (2, "retry(RetryConfig(10,RandomDelay(100 milliseconds)))"), (1, "objectify[String,String]"), (0, "http(s4)"))

    sample.e1.lines shouldBe List(
      (4, "enrich"),
      (3, "metrics(service1)"), (2, "logging(service1 using prefix someMessageName)"), (1, "objectify[String,String]"), (0, "http(s1)"),
      (2, "logging(service1 using prefix someMessageName)"), (1, "objectify[String,String]"), (0, "http(s2)"))


  }

}
