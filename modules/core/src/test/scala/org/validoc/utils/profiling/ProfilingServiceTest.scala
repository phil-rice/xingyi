package org.validoc.utils.profiling

import org.mockito.Mockito._
import org.validoc.utils.UtilsWithLoggingSpec
import org.validoc.utils.time.NanoTimeService

import scala.concurrent.Future
import scala.concurrent.duration.{Duration, _}
import scala.language.{implicitConversions, postfixOps}
import scala.util.Success

import org.validoc.utils.concurrency.AsyncForScalaFuture._
import org.validoc.utils._
class ProfilingServiceTest extends UtilsWithLoggingSpec {

  implicit def durationToNanos(d: Duration) = d.toNanos

  behavior of "ProfilingService"

  it should "time the delegate and report the duration to the TryProfileData" in {
    implicit val timeService = mock[NanoTimeService]
    val tryProfileData = mock[TryProfileData]


    val ps = new ProfilingService[Future, String, String]("name", Future.successful(_), tryProfileData)

    when(timeService.apply()) thenReturn(1 second, 3 second)
    ps("1").await shouldBe "1"
    verify(tryProfileData, times(1)).event(2 seconds)(Success("1"))
  }
}
