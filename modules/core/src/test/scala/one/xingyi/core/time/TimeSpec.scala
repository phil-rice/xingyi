package one.xingyi.core.time

import one.xingyi.core.UtilsWithLoggingSpec

import scala.concurrent.duration._
import scala.language.postfixOps
class TimeSpec extends UtilsWithLoggingSpec {
  behavior of "Delay"

  it should "have a delay within 50% of the specified duration" in {
    val rd = new RandomDelay(5 seconds)
    val duration1 = rd()
    val duration2 = rd()
    duration1 shouldNot equal(duration2)
    val seconds8 = (8 seconds).toNanos
    val seconds2 = (2 seconds).toNanos

    1 to 100 forall { _ =>
      val d = rd().toNanos
      d < seconds8 && d > seconds2
    }
  }

  behavior of "NanoTimeService"

  it should "return System.nanos" in {
    val x = implicitly[NanoTimeService].apply()
    val expected = System.nanoTime()

    Math.abs(x-expected) < 1000000000l shouldBe true
  }
}
