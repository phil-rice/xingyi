package org.validoc.utils.time

import java.util.concurrent.atomic.AtomicLong

import scala.language.postfixOps

class MockTimeService extends NanoTimeService {
  val i = new AtomicLong(1000)

  override def apply(): Long = i.addAndGet(100l)
}


