/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.strings

import one.xingyi.core.UtilsWithLoggingSpec


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
