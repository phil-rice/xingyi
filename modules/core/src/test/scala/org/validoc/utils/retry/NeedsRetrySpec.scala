package org.validoc.utils.retry

import org.validoc.utils.UtilsSpec

import scala.util.{Failure, Success}

class NeedsRetrySpec extends UtilsSpec {

  behavior of "Needs default needs retry"

  val needsRetry = implicitly[NeedsRetry[String]]
  it should "return false if everything was ok" in {
    needsRetry(Success(Right("someValue"))) shouldBe false
  }
  it should "return true if a failure occured" in {
    needsRetry(Success(Left("someFailure"))) shouldBe true
  }
  it should "return true if an exception occured" in {
    needsRetry(Failure(new RuntimeException)) shouldBe true
  }

}
