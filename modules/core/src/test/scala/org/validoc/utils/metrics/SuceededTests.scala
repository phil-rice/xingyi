package org.validoc.utils.metrics

import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success}

class SuceededTests extends FlatSpec with Matchers {

  val exception = new RuntimeException

  behavior of "DefaultSucceeded"

  it should "report a SuccessState if the try is Success, and a ExceptionState if the try is a failure" in {
    val suceeded = new DefaultSucceeded[String]
    suceeded(Success("someString")) shouldBe SuccessState("someString")
    suceeded(Failure(exception)) shouldBe ExceptionState(exception)
  }

  behavior of "SucceededFromFn"

  it should "report a SuccessState or FailedState if the try is Success depending on the function, and a ExceptionState if the try is a failure" in {
    val suceeded = new SucceededFromFn[String](_ == "success")
    suceeded(Success("success")) shouldBe SuccessState("success")
    suceeded(Success("failure")) shouldBe FailedState("failure")
    suceeded(Failure(exception)) shouldBe ExceptionState(exception)
  }
}