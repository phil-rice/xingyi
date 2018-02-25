package org.validoc.utils.tagless

import org.validoc.utils.UtilsSpec
import org.validoc.utils.http.HttpObjectFixture
import org.validoc.utils.logging.LogRequestAndResult
import org.validoc.utils.strings.IndentAndString
import org.validoc.utils.success.MessageName

import scala.language.higherKinds
import scala.language.implicitConversions

class TaglessSpec extends UtilsSpec with HttpObjectFixture {


  class Sample[Wrapper[_, _], Fail](implicit httpLanguage: TaglessLanguage[Wrapper, Fail, HttpReq, HttpRes]) {

    import httpLanguage._

    implicit def fn(s: String) = s;

    implicit val messageName = MessageName[String, String]("someMessageName")

    def s1 = http |+| objectify[String, String] |+| logging("service1") |+| metrics("service1")
    def s2 = http |+| objectify[String, String] |+| logging("service1")
    def s3 = http |+| objectify[String, String]
    def s4 = http |+| objectify[String, String]
    def m2 = merge2(s1, s2, (_: String, _: String, _: String) => "someFn")
    def m3 = merge3(s1, s2, s3, (_: String, _: String, _: String, _: String) => "someFn")
    def m4 = merge4(s1, s2, s3, s4, (_: String, _: String, _: String, _: String, _: String) => "someFn")
  }

  behavior of "Tagless with toString Interpreter"

  it should "handle" in {
    implicit val stringlanguage = new TaglessLanguageForToString[HttpReq, HttpRes]
    import stringlanguage._
    val sample = new Sample[StringHolder, Void]()
    sample.s1.lines shouldBe List((3, "metrics(service1)"), (2, "logging(service1 using prefixsomeMessageName)"), (1, "objectify[String,String]"), (0, "http"))

    sample.m2.lines shouldBe List((5, "merge2"),
      (3, "metrics(service1)"), (2, "logging(service1 using prefixsomeMessageName)"), (1, "objectify[String,String]"), (0, "http"),
      (2, "logging(service1 using prefixsomeMessageName)"), (1, "objectify[String,String]"), (0, "http"))
    //      (0,http), (1,objectify[String,String]), (2,logging(service1)), (3,metrics(service1)), (0,http), (1,objectify[String,String]), (2,logging(service1)))


    //    List()
    //    sample.m3.lines shouldBe List((0, "http"), (1, "objectify[String,String]"), (2, "logging(service1)"), (3, "metrics(service1)"))
    //    sample.m4.lines shouldBe List((0, "http"), (1, "objectify[String,String]"), (2, "logging(service1)"), (3, "metrics(service1)"))

  }

}
