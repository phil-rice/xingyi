package one.xingyi.utils.profiling

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

import one.xingyi.utils.strings.HasShortToString


class ProfilingBucket {
  val count = new AtomicInteger()
  val took = new AtomicLong()

  def event(duration: Long) {
    count.getAndIncrement()
    took.addAndGet(duration)
  }

  def clearData: Unit = {
    count.set(0)
    took.set(0)
  }

  def averageDurationInMs = if (count.get == 0) 0.0f else took.get().toFloat / count.get.toFloat / 1000000

  override def toString = f"$averageDurationInMs%,8.2fms/${count.get}%-4s"
}

class ProfileData extends HasShortToString {
  val all = new ProfilingBucket
  val under1ms = new ProfilingBucket
  val under100ms = new ProfilingBucket
  val under1s = new ProfilingBucket
  val rest = new ProfilingBucket

  val buckets = List(all, under1ms, under100ms, under1s, rest)

  def clearData = buckets.foreach(_.clearData)

  def event(duration: Long) {
    all.event(duration)
    val ms = duration / 1000000
    if (ms < 1) under1ms.event(duration)
    else if (ms < 100) under100ms.event(duration)
    else if (ms < 1000) under1s.event(duration)
    else rest.event(duration)
  }

  override def shortToString: String = buckets.map(_.toString).mkString(" ")
}
