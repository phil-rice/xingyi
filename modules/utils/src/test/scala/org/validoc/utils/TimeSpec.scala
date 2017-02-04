package org.validoc.utils

import org.validoc.utils.time.RandomDelay

import concurrent.duration._

class TimeSpec extends UtilsSpec {
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
}
