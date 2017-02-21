package org.validoc.utils.time

trait NanoTimeService {
  def apply(): Long
}

object SystemClockNanoTimeService extends NanoTimeService {
  override def apply(): Long = System.nanoTime()
}



