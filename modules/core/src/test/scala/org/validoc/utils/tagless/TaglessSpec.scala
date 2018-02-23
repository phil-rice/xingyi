package org.validoc.utils.tagless

import org.validoc.utils.UtilsSpec
import org.validoc.utils.http.HttpObjectFixture
import org.validoc.utils.strings.IndentAndString
import scala.language.higherKinds

class TaglessSpec extends UtilsSpec with HttpObjectFixture {


  class Sample[Wrapper[_, _]](implicit httpLanguage: TaglessLanguage[Wrapper, HttpReq, HttpRes]) {

    import httpLanguage._

    def s1 = http |+| objectify[String, String] |+| logging("service1") |+| metrics("service1")
    def s2 = http |+| objectify[String, String] |+| logging("service1")
    def s3 = http |+| objectify[String, String]
    def s4 = http |+| objectify[String, String]
    def m2 = merge2(s1, s2, (_: String, _, _) => "someFn")
    def m3 = merge3(s1, s2, s3, (_: String, _, _, _) => "someFn")
    def m4 = merge4(s1, s2, s3, s4, (_: String, _, _, _, _) => "someFn")
  }

  behavior of "Tagless with toString Interpreter"

  it should "handle" in {
    implicit val stringlanguage = new TaglessLanguageForToString[HttpReq, HttpRes]
    import stringlanguage._
    val sample = new Sample[StringHolder]()
    sample.s1.lines shouldBe List((0, "http"), (1, "objectify[String,String]"), (2, "logging(someName)"), (3, "metrics(someName)"))

    sample.m2.lines shouldBe List((0, "http"), (1, "objectify[String,String]"), (2, "logging(someName)"), (3, "metrics(someName)"))
    sample.m3.lines shouldBe List((0, "http"), (1, "objectify[String,String]"), (2, "logging(someName)"), (3, "metrics(someName)"))
    sample.m4.lines shouldBe List((0, "http"), (1, "objectify[String,String]"), (2, "logging(someName)"), (3, "metrics(someName)"))

  }

}
