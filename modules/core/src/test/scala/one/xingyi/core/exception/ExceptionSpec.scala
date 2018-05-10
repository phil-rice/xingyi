package one.xingyi.core.exception

import one.xingyi.core.UtilsSpec
import one.xingyi.core.exceptions.Exceptions

import scala.concurrent.Future
import one.xingyi.core.functions.AsyncForScalaFuture._
import ImplicitsForTest._
import one.xingyi.core.language.Language._

class ExceptionSpec extends UtilsSpec {

  behavior of "Exceptions"

  it should "catch an exception and add it to the monad" in {
    val runtimeException =  new RuntimeException("asd")
    val m: Future[String] = Exceptions[Future, String](throw runtimeException)
    intercept[RuntimeException](m.await()) shouldBe runtimeException

  }

}
