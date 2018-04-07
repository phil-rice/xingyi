package one.xingyi.utils.exception

import one.xingyi.utils.UtilsSpec
import one.xingyi.utils.exceptions.Exceptions

import scala.concurrent.Future
import one.xingyi.utils.functions.AsyncForScalaFuture._
import ImplicitsForTest._
import one.xingyi.utils.language.Language._

class ExceptionSpec extends UtilsSpec {

  behavior of "Exceptions"

  it should "catch an exception and add it to the monad" in {
    val runtimeException =  new RuntimeException("asd")
    val m: Future[String] = Exceptions[Future, String](throw runtimeException)
    intercept[RuntimeException](m.await()) shouldBe runtimeException

  }

}
