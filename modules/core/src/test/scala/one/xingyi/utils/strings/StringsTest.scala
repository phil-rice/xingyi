package one.xingyi.utils.strings

import one.xingyi.utils.UtilsWithLoggingSpec


class StringsTest extends UtilsWithLoggingSpec {

  "Strings.ellipse" should "return the string if the length is less than the number" in {
    Strings.ellipses(3)("") shouldBe ""
    Strings.ellipses(3)("abc") shouldBe "abc"
    Strings.ellipses(3)("abcd") shouldBe "abc.."
  }

  "Strings.removeWhiteSpace" should "remove whitespace" in {
    Strings.removeWhiteSpace("") shouldBe ""
    Strings.removeWhiteSpace("abc") shouldBe "abc"
    Strings.removeWhiteSpace("    a   b   c   ") shouldBe "abc"
    Strings.removeWhiteSpace("  a \tb\nc\f\r") shouldBe "abc"
  }

  "Strings.classNameOfObject" should "remove the $" in {
    Strings.classNameOfObject(Strings) shouldBe "Strings"
  }

  "Strings.lastSection" should "return the last section" in {
    Strings.lastSection("/")("a/b/c") shouldBe "c"
    Strings.lastSection("/")("") shouldBe ""
    Strings.lastSection("/")("a") shouldBe "a"
  }
  "Strings.allButlastSection" should "return the last section" in {
    Strings.allButlastSection("/")("a/b/c") shouldBe "a/b"
    Strings.allButlastSection("/")("") shouldBe ""
    Strings.allButlastSection("/")("a") shouldBe ""
    Strings.allButlastSection("/")("a/b") shouldBe "a"
  }
}
