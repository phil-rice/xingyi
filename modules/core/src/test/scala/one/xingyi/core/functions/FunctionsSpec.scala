package one.xingyi.core.functions

import one.xingyi.core.monad.LiftFunctionKleisli
import one.xingyi.core.strings.Strings
import one.xingyi.core.{FunctionFixture, UtilsSpec}

import scala.concurrent.Future

class FunctionsSpec extends UtilsSpec with FunctionFixture with LiftFunctionKleisli[Future] with ScalaFutureAsAsyncAndMonadAndFailer {

  "Functions.print" should "wrap a function, also printing the mssage" in {
    val f = fn("in", "out")
    val g = f andThen Functions.print[String](x => x + "printed")
    val (out, printed) = Strings.recordPrintln(g("in"))

    out shouldBe "out"
    printed.trim shouldBe "outprinted"
  }

  "Functions.identity" should "return the input" in {
    Functions.identity("a") shouldBe "a"
  }

  "LiftFunctionKleisli" should "turn function into kleisli" in {
    async.await(function("someName") { x: Int => x.toString } apply (3)) shouldBe "3"
  }
}
