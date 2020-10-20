/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.strings

import one.xingyi.core.UtilsWithLoggingSpec
import one.xingyi.core.orm.LinkUrl


class StringsTest extends UtilsWithLoggingSpec {

  "strings.split" should "split strings" in {
    Strings.split()("") shouldBe List()
    Strings.split()("a") shouldBe List("a")
    Strings.split()("a.b") shouldBe List("a", "b")
    Strings.split()(".a . b .d") shouldBe List("a", "b", "d")
  }

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

  "ShortPrint.apply" should "use the shortprint" in {
    implicit val shortPrintForInt: ShortPrint[Int] = i => i + "usedit"
    ShortPrint(1) shouldBe "1usedit"
  }

  "Strings.indentTuple" should "line up strings" in {
    val indent = Strings.indentTuple(".", 10, 20) _
    indent("one", "two") shouldBe
      //1234567890123456789012
      "..........one.......two"
    indent("o", "t") shouldBe
      "..........o.........t"
  }

  "Strings.trimChar" should "trim the char from both ends" in {
    val trim = Strings.trimChar('.') _
    trim("...ab.c...") shouldBe "ab.c"
    trim("ab.c...") shouldBe "ab.c"
    trim("...ab.c") shouldBe "ab.c"
    trim("......") shouldBe ""
    trim("") shouldBe ""
  }

  "Strings.clean" should "remove all but alphas_- and space" in {
    Strings.cleanString(" abc.$d/e\\g-h") shouldBe " abcdeg-h"
  }

  "Strings.uri" should "make a uri" in {
    Strings.uri("/abc", "def/", "/gh/", "i", "j") shouldBe "abc/def/gh/i/j"
  }

  "Strings.splitInTwo" should "return a tuple or an exception" in {

    val splitter = Strings.splitInTwo(":")
    splitter("one:int") shouldBe("one", "int")
    splitter("one: int") shouldBe("one", "int")
    splitter("one : int") shouldBe("one", "int")
    the[ParseException] thrownBy (splitter("one")) should have message ("Cannot split a string into two non empty parts using [:] string was [one]")
    the[ParseException] thrownBy (splitter("one:")) should have message ("Cannot split a string into two non empty parts using [:] string was [one:]")
    the[ParseException] thrownBy (splitter("one:abdc:a")) should have message ("Cannot split a string into two non empty parts using [:] string was [one:abdc:a]")

  }
  
  behavior of "Strings.extract"

  it should "extract data from paths" in {
    Strings.extractFromUrl("/{0}/path1/")("a/path1") shouldBe List("a")
    Strings.extractFromUrl("/{0}/path1/{1}")("a/path1/b") shouldBe List("a", "b")
    Strings.extractFromUrl("/{1}/path1/{0}")("a/path1/b") shouldBe List("b", "a")
    Strings.extractFromUrl("/{0}/path1/{1}/path2")("a/path1/b/path2") shouldBe List("a", "b")
    Strings.extractFromUrl("/prefix/{0}/path1/{1}/path2")("prefix/a/path1/b/path2") shouldBe List("a", "b")
    Strings.extractFromUrl("prefix/{0}/path1/{1}/path2")("prefix/a/path1/b/path2") shouldBe List("a", "b")
    Strings.extractFromUrl("some/path1/{0}")("some/path1/x") shouldBe List("x")
  }

  it should "work with or without leading or tailing /" in {
    Strings.extractFromUrl("{0}/path1/{1}/path2")("a/path1/b/path2") shouldBe List("a", "b")
    Strings.extractFromUrl("{0}/path1/{1}/path2")("a/path1/b/path2/") shouldBe List("a", "b")
    Strings.extractFromUrl("{0}/path1/{1}/path2/")("a/path1/b/path2") shouldBe List("a", "b")
    Strings.extractFromUrl("{0}/path1/{1}/path2/")("a/path1/b/path2/") shouldBe List("a", "b")

    Strings.extractFromUrl("/{0}/path1/{1}/path2")("a/path1/b/path2") shouldBe List("a", "b")
    Strings.extractFromUrl("/{0}/path1/{1}/path2")("a/path1/b/path2/") shouldBe List("a", "b")
    Strings.extractFromUrl("/{0}/path1/{1}/path2/")("a/path1/b/path2") shouldBe List("a", "b")
    Strings.extractFromUrl("/{0}/path1/{1}/path2/")("a/path1/b/path2/") shouldBe List("a", "b")
  }

  it should "reject paths that don't make sense - non consecutive paths" in {
    the[IllegalArgumentException] thrownBy (Strings.extractFromUrl("/{0}/path1/{3}/path2/")) should have message ("requirement failed: Must have consecutive {0}/{1} etc in template: [/{0}/path1/{3}/path2/] dataNumbers List((0,0), (3,2))")
  }
  it should "reject paths that don't make sense - non numeric items" in {
    the[RuntimeException] thrownBy (Strings.extractFromUrl("/{id}/path1/{0}/path2/")) should have message ("Error finding the numbers in template [/{id}/path1/{0}/path2/]")
  }
  it should "reject paths that don't make sense - fillers with { or }" in {
    the[IllegalArgumentException] thrownBy (Strings.extractFromUrl("/{0}/pat{h1/{1}/")) should have message ("requirement failed: Cannot have { or } in the template [/{0}/pat{h1/{1}/] except in format /{n}/.")
    the[IllegalArgumentException] thrownBy (Strings.extractFromUrl("/{0}/pat}h1/{1}/")) should have message ("requirement failed: Cannot have { or } in the template [/{0}/pat}h1/{1}/] except in format /{n}/.")
  }

  it should "return an empty list if it doesn't match" in {
    Strings.extractFromUrl("/{0}/path1/{1}/path2")("a/notpath1/b/path2") shouldBe List()
    Strings.extractFromUrl("/{0}/path1/{1}/path2")("a/path1/b/notpath2") shouldBe List()
    Strings.extractFromUrl("/{0}/path1/{1}/path2")("predix/a/path1/b/path2") shouldBe List()
    Strings.extractFromUrl("/{0}/path1/{1}/path2")("a/path1/b/path2/post") shouldBe List()
  }


}
