package org.validoc.utils.exception

import org.validoc.utils.UtilsSpec
import org.validoc.utils.exceptions.Exceptions

import scala.concurrent.Future
import org.validoc.utils.functions.AsyncForScalaFuture._
import ImplicitsForTest._
import org.validoc.utils.language.Language._

class ExceptionSpec extends UtilsSpec {

  behavior of "Exceptions"

  it should "catch an exception and add it to the monad" in {
    val runtimeException =  new RuntimeException("asd")
    val m: Future[String] = Exceptions[Future, String](throw runtimeException)
    intercept[RuntimeException](m.await()) shouldBe runtimeException

  }

}
