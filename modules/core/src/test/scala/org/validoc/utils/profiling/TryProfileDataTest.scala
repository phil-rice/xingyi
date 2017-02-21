package org.validoc.utils.profiling

import org.validoc.utils.UtilsSpec

import scala.concurrent.duration._
import scala.util.{Failure, Success}

class TryProfileDataTest extends UtilsSpec {
  behavior of "TryProfileData"

  implicit def durationToNanos(d: Duration) = d.toNanos

  val success = Success(1)
  val failure = Failure(new RuntimeException)
  it should "separately record success and failure times" in {
    val pd = new TryProfileData

    pd.event(100 micros)(success)
    pd.event(100 micros)(success)
    pd.event(10 millis)(failure)
    pd.event(10 millis)(failure)
    pd.event(10 millis)(success)
    pd.event(200 millis)(failure)
    pd.event(200 millis)(success)
    pd.event(2 second)(failure)
    pd.event(3 second)(success)
    pd.succeededData.shortToString shouldBe "  642.04ms/5        0.10ms/2       10.00ms/1      200.00ms/1    3,000.00ms/1   "
    pd.failedData.shortToString shouldBe "  555.00ms/4        0.00ms/0       10.00ms/2      200.00ms/1    2,000.00ms/1   "
  }

  it should "clearData and allow more recordings" in {
    val pd = new TryProfileData

    pd.event(100 micros)(success)
    pd.event(100 micros)(success)
    pd.event(10 millis)(failure)
    pd.event(10 millis)(failure)
    pd.event(10 millis)(success)
    pd.event(200 millis)(failure)
    pd.event(200 millis)(success)
    pd.clearData
    pd.event(2 second)(failure)
    pd.event(3 second)(success)
    pd.succeededData.shortToString shouldBe "3,000.00ms/1        0.00ms/0        0.00ms/0        0.00ms/0    3,000.00ms/1   "
    pd.failedData.shortToString shouldBe "2,000.00ms/1        0.00ms/0        0.00ms/0        0.00ms/0    2,000.00ms/1   "
  }
}
