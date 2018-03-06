package org.validoc.utils.functions

import org.validoc.utils.strings.Strings
import org.validoc.utils.{FunctionFixture, UtilsSpec}

class FunctionsSpec extends UtilsSpec with FunctionFixture {

  "Functions.print" should "wrap a function, also printing the mssage" in {
    val f = fn("in", "out")
    val g = f andThen Functions.print[String](x => x + "printed")
    val (out, printed) = Strings.recordPrintln(g("in"))

    out shouldBe "out"
    printed.trim shouldBe "outprinted"
  }

  "Functions.identity" should "return the input" in {
    Functions.identify("a") shouldBe "a"
  }

}
