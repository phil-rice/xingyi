/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.profiling

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

import one.xingyi.core.strings.HasShortToString


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
