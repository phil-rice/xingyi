package one.xingyi.core.profiling

import one.xingyi.core.UtilsWithLoggingSpec

import scala.concurrent.duration.{Duration, _}
import scala.language.{implicitConversions, postfixOps}

class ProfileDataTest extends UtilsWithLoggingSpec {
  behavior of "ProfileData"

  implicit def durationToNanos(d: Duration) = d.toNanos

  it should "put the durations into different buckets" in {
    val pd = new ProfileData
    pd.event(100 micros)
    pd.event(100 micros)
    pd.event(10 millis)
    pd.event(10 millis)
    pd.event(10 millis)
    pd.event(200 millis)
    pd.event(200 millis)
    pd.event(2 second)
    pd.event(2 second)
    pd.shortToString shouldBe "  492.24ms/9        0.10ms/2       10.00ms/3      200.00ms/2    2,000.00ms/2   "
  }

  it should "clear data and then accumulate data again" in {
    val pd = new ProfileData
    pd.event(10 millis)
    pd.event(200 millis)
    pd.clearData
    pd.event(1 second)
    pd.shortToString shouldBe "1,000.00ms/1        0.00ms/0        0.00ms/0        0.00ms/0    1,000.00ms/1   "

  }
}


