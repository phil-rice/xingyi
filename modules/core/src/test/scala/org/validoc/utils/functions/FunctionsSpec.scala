package org.validoc.utils.functions

import org.validoc.utils.strings.Strings
import org.validoc.utils.{FunctionFixture, UtilsSpec}

import scala.concurrent.Future

class FunctionsSpec extends UtilsSpec with FunctionFixture with LiftFunctionKleisli[Future] with ScalaFutureAsAsyncAndMonad {

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

  "LiftFunctionKleisli" should "turn function into kleisli" in {
    async.await(function("someName") { x: Int => x.toString } apply (3)) shouldBe "3"
  }
}
