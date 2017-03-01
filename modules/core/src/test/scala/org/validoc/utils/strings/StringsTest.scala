package org.validoc.utils.strings

import org.validoc.utils.UtilsWithLoggingSpec
import org.validoc.utils.strings.Strings


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

}
